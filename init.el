;;; package --- Emacs configurations
;;; Commentary:

;;; Code:

;;; Helpers
(defun osx-p ()
  "Check if a system is running OSX."
  (eq system-type 'darwin))

(defun linux-p ()
  "Check if a system is running Linux."
  (eq system-type 'gnu/linux))

(defun window-p ()
  "Check if a system is running Windows."
  (eq system-type 'windows-nt))

(defun relative-path (path)
  "Return the full path of a file (PATH) in the user's Emacs directory."
  (concat (file-name-directory (or load-file-name buffer-file-name)) path))

(defun apply-function-to-region (fn)
  "Apply function to region given FN."
  (let* ((beg (region-beginning))
         (end (region-end))
         (resulting-text
          (funcall fn (buffer-substring-no-properties beg end))))
    (kill-region beg end)
    (insert resulting-text)))

(defun move-region (start end n)
  "Move the current region up or down by N lines."
  (interactive "r\np")
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (let ((start (point)))
      (insert line-text)
      (setq deactivate-mark nil)
      (set-mark start))))

(defun move-region-up (start end n)
  "Move the current line up by N lines."
  (interactive "r\np")
  (move-region start end (if (null n) -1 (- n))))

(defun move-region-down (start end n)
  "Move the current line down by N lines."
  (interactive "r\np")
  (move-region start end (if (null n) 1 n)))

(defun pye/kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer
    (delq (current-buffer)
      (cl-remove-if-not 'buffer-file-name (buffer-list)))))

(defun pye/now ()
  "Insert the current timestamp."
  (interactive)
  (insert (format-time-string "%Y-%m-%dT%H:%M:%S")))

(defun pye/open-current-directory-in-finder ()
  (interactive)
  (shell-command (format "open -R %s" (file-name-directory (buffer-file-name)))))

(defun pye/load-file-from-url (url)
  (condition-case e
    (save-window-excursion
      (eval-buffer
        (browse-url-emacs url)))
    (error (message "Could not load remote library: %s" (cadr e)))))

(defun pye/dired-get-size ()
  (interactive)
  (let ((files (dired-get-marked-files)))
    (with-temp-buffer
      (apply 'call-process "/usr/bin/du" nil t nil "-sch" files)
      (message "Size of all marked files: %s"
        (progn
          (re-search-backward "\\(^[0-9.,]+[A-Za-z]+\\).*total$")
          (match-string 1))))))

(defun pye/copy-buffer-file-name-to-clipboard ()
  "Copies the buffer file name to the clipboard"
  (interactive)
  (let ((buf-name (buffer-file-name)))
    (if buf-name
        (with-temp-buffer
          (insert buf-name)
          (copy-region-as-kill (point-min) (point-max))
          (message "Copied %s to clipboard" buf-name))
      (message "Your buffer is not backed by a file"))))

(defun pye/uuid ()
  (interactive)
  (insert
    (downcase (string-trim-right
                (shell-command-to-string "uuidgen")))))

;;; Package setup
;;; straight
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(straight-pull-recipe-repositories)

;; Increasing the minimum prime bits size to something larger
;; than the default settings stops all the GnuTLS warnings from
;; showing up. This might not be the right place, but it needs
;; to happen before we install packages.
(setq byte-compile-warnings nil)
(setq gnutls-min-prime-bits 4096)

;;;; Common packages
;; stop creating backup~ file ;;;;
(setq make-backup-files nil)

(when (linux-p)
  (setq browse-url-browser-function 'browse-url-xdg-open))

;; use emacs pinenttry for EasyPG
(setq epa-pinentry-mode 'loopback)

;;; disable auto revert
(global-auto-revert-mode nil)

(use-package dired-single)
(setq dired-dwim-target t)
(add-hook 'dired-mode-hook
  (lambda ()
    (dired-hide-details-mode)
    (define-key dired-mode-map [return] 'dired-single-buffer)
    (define-key dired-mode-map [backspace] 'dired-single-up-directory)
    (define-key dired-mode-map (kbd "?") 'pye/dired-get-size)))

(setq uniquify-buffer-name-style 'forward)

;; tramp
(setq tramp-default-method "ssh")
;;;; configure TRAMP to respect the PATH variable on the remote machine
(cond
  ((boundp 'tramp-remote-path)
    (add-to-list 'tramp-remote-path 'tramp-own-remote-path)))

(use-package avy
  :config
  (setq avy-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s)))

(use-package ag
  :defer t)

(use-package projectile)

(use-package switch-window
  :config
  (define-key switch-window-extra-map (kbd "c") 'switch-window-mvborder-up)
  (define-key switch-window-extra-map (kbd "t") 'switch-window-mvborder-down)
  (define-key switch-window-extra-map (kbd "h") 'switch-window-mvborder-left)
  (define-key switch-window-extra-map (kbd "n") 'switch-window-mvborder-right)
  (global-set-key (kbd "M-p") 'switch-window))

(use-package exec-path-from-shell
  ;; https://github.com/purcell/exe...
  ;; only use exec-path-from-shell on OSX
  ;; this hopefully sets up path and other vars better
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package expand-region)

(use-package magit
  :bind ("C-c C-g" . magit-status)
  :config
  (setq transient-default-level 5))

(use-package forge
  :after magit)

(use-package git-link)

(use-package smartparens
  :config
  (sp-local-tag '(sgml-mode web-mode) "<" "<_>" "</_>" :transform 'sp-match-sgml-tags)
  (setq show-paren-delay 0)
  (show-paren-mode 1)
  (smartparens-global-mode t))

(use-package vlf
  :defer t)

(use-package string-inflection)

(use-package restclient
  :mode ("\\.rest\\'" . restclient-mode))

(add-hook 'term-mode-hook
  (lambda () (setq show-trailing-whitespace nil)))

(add-hook 'vterm-mode-hook
  (lambda () (setq show-trailing-whitespace nil)))

(use-package yasnippet
  :config
  (yas-global-mode 1)
  (add-hook 'yas-minor-mode-hook
    (lambda () (yas-activate-extra-mode 'fundamental-mode))))

(use-package ivy-yasnippet)

;;;; settings
(setq settings-dir (relative-path "settings"))
(mapc 'load
  (mapcar
    (lambda (name) (concat settings-dir "/" name ".el"))
    '(
       "appearance"
       "completion"
       "keybinding"
       "dockerfile"
       "javascript"
       "python"
       "markdown"
       "graphql"
       "sql"
       "org"
       "intellisense"
       )))

(eldoc-mode 0)

;; code folding
(defun toggle-selective-display (column)
  (interactive "P")
  (set-selective-display
    (or column
      (unless selective-display
        (1+ (current-column))))))

(defun toggle-hiding (column)
  (interactive "P")
  (if hs-minor-mode
    (if (condition-case nil
          (hs-toggle-hiding)
          (error t))
      (hs-show-all))
    (toggle-selective-display column)))
(global-set-key (kbd "C-'") 'toggle-hiding)
(global-set-key (kbd "C-,") 'toggle-selective-display)

;; default regex engine
(use-package re-builder
  :config
  (setq reb-re-syntax 'string))

;; search and edit
(use-package wgrep)

(use-package wgrep-ag
  :config
  (add-hook 'ag-mode-hook 'wgrep-ag-setup))

;;;; Indentation
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq css-indent-offset 2)
(setq python-indent-offset 4)
(setq py-indent-offset 4)
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-code-indent-level 2)
(setq web-mode-markup-indent-offset 2)
(setq lisp-indent-offset 2)

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 100 1000 1000))
(setq read-process-output-max (* 1024 1024))
