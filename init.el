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

(defun pye/ivy-scratch ()
  "Open scratch file"
  (interactive)
  (find-file
    (format
      "~/Dropbox/Notes/scratch.%s"
      (ivy-read
        "scratch: "
        '("org.txt" "sql" "rest")))))

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

(defun full-comment-box (b e)
  "Draw a box comment around the region but arrange for the region
  to extend to at least the fill column. Place the point after the
  comment box."
  (interactive "r")
  (let ((e (copy-marker e t)))
    (goto-char b)
    (end-of-line)
    (insert-char ?  (- fill-column (current-column)))
    (comment-box b e 1)
    (goto-char e)
    (set-marker e nil)))

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
(defun pye/dired-get-size ()
  (interactive)
  (let ((files (dired-get-marked-files)))
    (with-temp-buffer
      (apply 'call-process "/usr/bin/du" nil t nil "-sch" files)
      (message "Size of all marked files: %s"
        (progn
          (re-search-backward "\\(^[0-9.,]+[A-Za-z]+\\).*total$")
          (match-string 1))))))

(use-package dired-single)

(setq dired-dwim-target t)
(add-hook 'dired-mode-hook
  (lambda ()
    (dired-hide-details-mode)
    (define-key dired-mode-map [return] 'dired-single-buffer)
    (define-key dired-mode-map [backspace] 'dired-single-up-directory)
    (define-key dired-mode-map (kbd "?") 'pye/dired-get-size)))

(setq uniquify-buffer-name-style 'forward)

(use-package ivy
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (global-set-key (kbd "<f6>") 'ivy-resume)
  (define-key ivy-minibuffer-map (kbd "C-j") #'ivy-immediate-done)
  (define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)
  (ivy-mode 1))

(use-package swiper
  :config
  (setq ivy-display-style 'fancy)
  (setq avy-background t)
  (setq avy-all-windows t))

(use-package avy
  :config
  (setq avy-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s)))

(use-package rg
  :defer t)

(use-package counsel
  :config
  (global-set-key (kbd "M-x") 'counsel-M-x))

(use-package counsel-projectile
  :ensure projectile
  :init
  (setq projectile-completion-system 'ivy))

(use-package switch-window
  :config
  (define-key switch-window-extra-map (kbd "c") 'switch-window-mvborder-up)
  (define-key switch-window-extra-map (kbd "t") 'switch-window-mvborder-down)
  (define-key switch-window-extra-map (kbd "h") 'switch-window-mvborder-left)
  (define-key switch-window-extra-map (kbd "n") 'switch-window-mvborder-right)
  (global-set-key (kbd "M-p") 'switch-window))

(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-show-numbers t)
  (setq company-dabbrev-downcase nil))

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
  :defer t
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package magit
  :bind ("C-c C-g" . magit-status)
  :config
  (setq transient-default-level 5))

(use-package smartparens
  :defer t
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
  (lambda ()
    (setq show-trailing-whitespace nil)))

(use-package yankpad
  :ensure t
  :defer t
  :init
  (setq yankpad-file "~/Dropbox/Notes/yankpad.org"))

(use-package yasnippet
  :config
  (add-hook 'prog-mode-hook #'yas-minor-mode))

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
       )))

(eldoc-mode 0)


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
