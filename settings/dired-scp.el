;;; dired-scp.el --- Build an scp command for the files marked in dired  -*- lexical-binding: t; -*-

;;; Commentary:

;; `dired-scp-download-command' builds the scp invocation that copies the
;; files marked in a dired buffer down to your local machine, and puts it in
;; the kill ring so you can paste it into a local terminal.
;;
;; It handles both arrangements:
;;
;;   - Emacs running ON the remote host (plain local dired).  The ssh target
;;     comes from `dired-scp-default-target', falling back to the system name.
;;   - Emacs running locally with a TRAMP dired buffer.  The user, host and
;;     port are taken from the TRAMP file name.
;;
;; `dired-scp-upload' goes the other way: it uploads the marked (local) files
;; to a host picked from the Host entries of your ssh config.  The remote
;; destination directory is browsed with completion over TRAMP, and the scp
;; command is offered for editing before it runs in `*dired-scp-upload*'.

;;; Code:

(require 'dired)
(require 'tramp)

(defgroup dired-scp nil
  "Generate scp commands from dired."
  :group 'dired)

(defcustom dired-scp-default-target nil
  "SSH target used for non-TRAMP (local) files.
Either a plain host name or USER@HOST, matching an entry in your
~/.ssh/config.  When nil, `system-name' is used."
  :type '(choice (const :tag "Use `system-name'" nil) string)
  :group 'dired-scp)

(defcustom dired-scp-destination "~/Downloads/"
  "Default local destination directory offered in the prompt."
  :type 'string
  :group 'dired-scp)

(defcustom dired-scp-ssh-config-files '("~/.ssh/config")
  "Ssh config files whose Host entries are offered as upload targets.
Include directives inside them are followed."
  :type '(repeat file)
  :group 'dired-scp)

(defcustom dired-scp-upload-destination "~/"
  "Default remote directory offered when uploading.
Interpreted relative to the remote host, so ~/ is the remote home."
  :type 'string
  :group 'dired-scp)

(defcustom dired-scp-tramp-method "ssh"
  "TRAMP method used to browse the remote host for the upload destination."
  :type 'string
  :group 'dired-scp)

(defvar dired-scp-host-history nil
  "Minibuffer history of hosts chosen by `dired-scp-upload'.")

(defun dired-scp--remote-quote (path)
  "Quote PATH for scp.
scp expands the remote path through a shell on the far side, so it needs
one round of shell quoting beyond what the local shell consumes."
  (shell-quote-argument (shell-quote-argument path)))

(defun dired-scp--local-quote (path)
  "Quote PATH for the local shell, leaving a leading ~/ or ~user/ intact.
`shell-quote-argument' escapes the tilde, which would make scp write to a
directory literally named \"~\" instead of your home directory."
  (if (string-match "\\`\\(~[^/]*/\\)\\(.*\\)\\'" path)
      (concat (match-string 1 path)
              (let ((rest (match-string 2 path)))
                (if (string-empty-p rest) "" (shell-quote-argument rest))))
    (shell-quote-argument path)))

(defun dired-scp--spec (file)
  "Return a plist describing FILE as (:target T :port P :path LOCALNAME)."
  (if (file-remote-p file)
      (with-parsed-tramp-file-name file parsed
        (list :target (concat (when parsed-user (concat parsed-user "@"))
                              ;; Strip the IPv6 brackets TRAMP adds.
                              (replace-regexp-in-string "\\`\\[\\|\\]\\'" ""
                                                        (or parsed-host "")))
              :port parsed-port
              :path parsed-localname))
    (list :target (or dired-scp-default-target (system-name))
          :port nil
          :path (expand-file-name file))))

;;;###autoload
(defun dired-scp-download-command (&optional arg)
  "Build an scp command downloading the marked files to the local machine.

The command is echoed and pushed onto the kill ring; nothing is executed,
since the download has to run on your local machine, not this one.

With a prefix ARG, operate on the next ARG files instead of the marked
ones, exactly as other dired commands do."
  (interactive "P")
  (let* ((files (dired-get-marked-files nil arg))
         (specs (mapcar #'dired-scp--spec files))
         (targets (delete-dups (mapcar (lambda (s) (plist-get s :target)) specs)))
         (ports (delete-dups (mapcar (lambda (s) (plist-get s :port)) specs))))
    (unless files
      (user-error "No files marked"))
    (unless (= 1 (length targets))
      (user-error "Marked files span several hosts: %s" (string-join targets ", ")))
    (unless (= 1 (length ports))
      (user-error "Marked files span several ports"))
    (let* ((target (car targets))
           (port (car ports))
           (remote-p (file-remote-p (car files)))
           ;; Only complete the destination when Emacs is running on the
           ;; machine that will receive the files.  With a TRAMP buffer that
           ;; is true; with a plain local buffer on the remote host it is
           ;; not, and completing would offer *this* host's directories.
           (dest (if remote-p
                     (abbreviate-file-name
                      (read-directory-name "Download to (on this machine): "
                                           dired-scp-destination nil nil))
                   (read-string "Download to (path on your local machine): "
                                dired-scp-destination)))
           (sources
            (mapcar (lambda (s)
                      (format "%s:%s" target
                              (dired-scp--remote-quote (plist-get s :path))))
                    specs))
           (command
            (string-join
             (append '("scp")
                     ;; scp wants -P for the port; ssh wants -p.
                     (when port (list "-P" (format "%s" port)))
                     (when (cdr sources) '("-C"))
                     sources
                     (list (dired-scp--local-quote dest)))
             " ")))
      (kill-new command)
      (message "Copied to kill ring — run this locally:\n%s" command)
      command)))

(defun dired-scp--ssh-config-hosts (files &optional depth)
  "Return the concrete Host names declared in the ssh config FILES.
Wildcard patterns
\(containing *, ? or a leading !) are skipped, since they cannot be
connected to.  Include directives are followed up to a small DEPTH."
  (let ((depth (or depth 0))
        hosts)
    (dolist (file files)
      (setq file (expand-file-name file))
      (when (and (< depth 8) (file-readable-p file))
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (while (re-search-forward
                  "^[ \t]*\\(host\\|include\\)\\(?:[ \t]*=[ \t]*\\|[ \t]+\\)\\(.*\\)$"
                  nil t)
            (let ((keyword (downcase (match-string 1)))
                  ;; Drop trailing comments, then split into words,
                  ;; honouring double quotes.
                  (args (split-string-and-unquote
                         (replace-regexp-in-string "[ \t]*#.*\\'" ""
                                                   (match-string 2)))))
              (if (equal keyword "host")
                  (dolist (h args)
                    (unless (string-match-p "[*?]\\|\\`!" h)
                      (push h hosts)))
                ;; Relative Include paths are resolved against ~/.ssh.
                (dolist (pattern args)
                  (let ((default-directory (expand-file-name "~/.ssh/")))
                    (setq hosts
                          (append (reverse
                                   (dired-scp--ssh-config-hosts
                                    (file-expand-wildcards
                                     (expand-file-name pattern) t)
                                    (1+ depth)))
                                  hosts))))))))))
    (delete-dups (nreverse hosts))))

(defun dired-scp--read-host ()
  "Read an ssh host, completing on the Host entries of the ssh config."
  (let ((hosts (dired-scp--ssh-config-hosts dired-scp-ssh-config-files)))
    (completing-read (format-prompt "Upload to host" (car dired-scp-host-history))
                     hosts nil nil nil 'dired-scp-host-history
                     (car dired-scp-host-history))))

(defun dired-scp--read-remote-directory (host)
  "Browse HOST over TRAMP and return the chosen directory's remote path."
  (let* ((root (format "/%s:%s:" dired-scp-tramp-method host))
         (dir (read-directory-name (format "Upload to directory on %s: " host)
                                   (concat root dired-scp-upload-destination)
                                   nil nil)))
    (unless (string-prefix-p root dir)
      (user-error "Destination %s is not on %s" dir host))
    ;; Expanding over TRAMP resolves ~ against the remote home, so the
    ;; path can be quoted for scp without a literal "~" surviving.
    (file-name-as-directory (file-remote-p (expand-file-name dir) 'localname))))

;;;###autoload
(defun dired-scp-upload (&optional arg)
  "Upload the marked files to a host chosen from your ssh config.

The host is completed from the Host entries in `dired-scp-ssh-config-files',
and the destination directory is browsed on that host over TRAMP.  The scp
command is then offered for editing, pushed onto the kill ring, and run
asynchronously in the `*dired-scp-upload*' buffer, where any password
prompt can be answered.

With a prefix ARG, operate on the next ARG files instead of the marked
ones, exactly as other dired commands do."
  (interactive "P")
  (let ((files (dired-get-marked-files nil arg)))
    (unless files
      (user-error "No files marked"))
    (when (seq-some #'file-remote-p files)
      (user-error "Upload works on local files; these are on %s"
                  (file-remote-p (car files))))
    (let* ((host (dired-scp--read-host))
           (dest (dired-scp--read-remote-directory host))
           (command
            (string-join
             (append '("scp")
                     (when (seq-some #'file-directory-p files) '("-r"))
                     (when (cdr files) '("-C"))
                     (mapcar (lambda (f)
                               (shell-quote-argument (expand-file-name f)))
                             files)
                     (list (format "%s:%s" host
                                   (dired-scp--remote-quote dest))))
             " "))
           (command (read-shell-command "Upload command: " command)))
      (kill-new command)
      ;; Run from a local directory: a TRAMP `default-directory' would run
      ;; scp on the remote host instead.
      (let ((default-directory (expand-file-name "~/")))
        (async-shell-command command "*dired-scp-upload*"))
      command)))

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "C-c C-s") #'dired-scp-download-command)
  (define-key dired-mode-map (kbd "C-c C-u") #'dired-scp-upload))

(provide 'dired-scp)
;;; dired-scp.el ends here
