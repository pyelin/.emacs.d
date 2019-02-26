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

(defun server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server."
  (interactive)
  (save-some-buffers)
  (kill-emacs))

(defun executable-find (command)
  "Search for COMMAND in `exec-path' and return the absolute file name.
Return nil if COMMAND is not found anywhere in `exec-path'."
  ;; Use 1 rather than file-executable-p to better match the behavior of
  ;; call-process.
  (interactive "executable:")
  (locate-file command exec-path exec-suffixes 1))


(defun fib (n)
  "Calculate fibonacci number, given the index as N."
  (cond
   ((= n 1) 1)
   ((= n 2) 1)
   (t (+ (fib (- n 1)) (fib (- n 2))))))

(defun collatz (n)
  "Collatz conjecture: no matter what number as N you start with, you will always eventually reach 1."
  (let ((next (cond ((= (% n 2) 0) (/ n 2))
                  (t (+ (* n 3) 1)))))
       (cond ((> next 1) (collatz next))
             (t next))))

(defvar-local display-images t)
(defun toggle-image-display ()
  "Toggle images display on current buffer."
  (interactive)
  (setq display-images (null display-images))
  (backup-display-property display-images))

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


;;; Package setup
;; Have to make sure it's loaded before we do anything with it.
(require 'package)

;; Set up the package repos
(setq package-archives '(("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("melpa"        . "https://melpa.org/packages/")
                         ("gnu"          . "https://elpa.gnu.org/packages/")))
;; Make sure we load what we need.
(setq package-enable-at-startup nil)
(package-initialize)

;; Set the initial state to non-refreshed. This can also be set back
;; to nil if we want to run a refresh on the next install.
(defvar refreshed-package-list nil)

(defun ensure-refreshed ()
  "Ensure the package list has been refreshed this startup."
  (unless refreshed-package-list
    (package-refresh-contents)
    (setq refreshed-package-list t)))

(defun package-ensure-installed (package)
  "Install a missing PACKAGE if it isn't already."
  (unless (package-installed-p package)
    (package-install package)))

;; wrap package-install to make sure that the first install
;; of each session will refresh the package list
(advice-add 'package-install
            :before
            (lambda (&rest args)
              (ensure-refreshed)))

;; Increasing the minimum prime bits size to something larger
;; than the default settings stops all the GnuTLS warnings from
;; showing up. This might not be the right place, but it needs
;; to happen before we install packages.
(setq byte-compile-warnings nil
      gnutls-min-prime-bits 4096)

(package-ensure-installed 'use-package)
(eval-when-compile
  (defvar use-package-verbose t)
  (require 'use-package))

(setq use-package-always-ensure t)

;;;; Common packages
;; stop creating backup~ file ;;;;
(setq make-backup-files nil)

(when (linux-p)
  (setq browse-url-browser-function 'browse-url-xdg-open))

;; use emacs pinenttry for EasyPG
(setq epa-pinentry-mode 'loopback)

;;;; dired
(defun dired-get-size ()
  (interactive)
  (let ((files (dired-get-marked-files)))
    (with-temp-buffer
      (apply 'call-process "/usr/bin/du" nil t nil "-sch" files)
      (message "Size of all marked files: %s"
        (progn
          (re-search-backward "\\(^[0-9.,]+[A-Za-z]+\\).*total$")
          (match-string 1))))))

(defun xah-open-in-external-app (&optional @fname)
  "Open the current file or dired marked files in external app.
The app is chosen from your OS's preference.

When called in emacs lisp, if @fname is given, open that.

URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2019-01-18"
  (interactive)
  (let* (
         ($file-list
          (if @fname
              (progn (list @fname))
            (if (string-equal major-mode "dired-mode")
                (dired-get-marked-files)
              (list (buffer-file-name)))))
         ($do-it-p (if (<= (length $file-list) 5)
                       t
                     (y-or-n-p "Open more than 5 files? "))))
    (when $do-it-p
      (cond
       ((string-equal system-type "windows-nt")
        (mapc
         (lambda ($fpath)
           (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" $fpath t t))) $file-list))
       ((string-equal system-type "darwin")
        (mapc
         (lambda ($fpath)
           (shell-command
            (concat "open " (shell-quote-argument $fpath))))  $file-list))
       ((string-equal system-type "gnu/linux")
        (mapc
         (lambda ($fpath) (let ((process-connection-type nil))
                            (start-process "" nil "xdg-open" $fpath))) $file-list))))))

(add-hook 'dired-mode-hook
  (lambda ()
    (dired-hide-details-mode)
    (define-key dired-mode-map [return] 'dired-single-buffer)
    (define-key dired-mode-map [backspace] 'dired-single-up-directory)
    (define-key dired-mode-map [o] 'xah-open-in-external-app)))


(use-package ibuffer)

(use-package ivy
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (global-set-key (kbd "<f6>") 'ivy-resume)
  (ivy-mode 1))

(use-package swiper
  :config
  (setq ivy-display-style 'fancy))

(use-package counsel
  :config
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (setq counsel-ag-base-command "ag --nocolor --nogroup --smart-case --ignore-dir=public %s"))

(use-package counsel-projectile
  :ensure projectile
  :init
  (setq projectile-completion-system 'ivy))

(use-package switch-window
  :config
  (global-set-key (kbd "M-p") 'switch-window))

(use-package ag
  :init
  (setq ag-reuse-window 't)  ;; use same result buffer
  (setq ag-highlight-search t)
  (setq ag-arguments '("--smart-case" "--stats" "--ignore-dir=public"))
  :defer t)

(use-package wgrep-ag
  :defer t)

(use-package smooth-scroll)

(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-tern))
  (setq company-show-numbers t))

(use-package dumb-jump)

(use-package exec-path-from-shell
  ;; https://github.com/purcell/exe...
  ;; only use exec-path-from-shell on OSX
  ;; this hopefully sets up path and other vars better
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

(use-package expand-region)

(use-package flycheck
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package magit
  :bind ("C-c C-g" . magit-status))

(use-package smartparens
  :config
  (sp-local-tag '(sgml-mode web-mode) "<" "<_>" "</_>" :transform 'sp-match-sgml-tags)
  (setq show-paren-delay 0)
  (show-paren-mode 1)
  (smartparens-global-mode t))

(use-package vlf)

(use-package string-inflection)

(use-package yasnippet
  :config
  (setq yas-snippet-dirs (list "~/.emacs.d/snippets" (relative-path "yasnippets")))
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode))

(use-package ivy-yasnippet)

(use-package restclient
  :mode ("\\.rest\\'" . restclient-mode))

;;;; tramp
;; eg. ssh:test@host#2222:/tmp
(setq tramp-default-method "ssh")

;;;; settings
(setq settings-dir (relative-path "settings"))
(mapc 'load
  (mapcar
    (lambda (name) (concat settings-dir "/" name ".el"))
    '(
       "appearance"
       "keybinding"
       "dockerfile"
       "javascript"
       "python"
       "markdown"
       "org"
       "rust"
     )))


;;;; Indentation
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq css-indent-offset 2)
(setq python-indent-offset 2)
(setq js2-indent-offset 2)
(setq js2-indent-level 2)
(setq js2-highlight-level 3)
(setq js2-basic-offset 2)
(setq js-indent-offset 2)
(setq js-indent-level 2)
(setq jsx-indent-level 2)
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-code-indent-level 2)
(setq web-mode-markup-indent-offset 2)
(setq lisp-indent-offset 2)
