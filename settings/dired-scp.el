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

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "C-c C-s") #'dired-scp-download-command))

(provide 'dired-scp)
;;; dired-scp.el ends here
