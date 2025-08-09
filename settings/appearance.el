;;; package --- All configs related to appearance
;;; Code:
;;; Commentary:

(use-package doom-themes
  :config
  (load-theme 'doom-vibrant t)
  (setq-default cursor-type '(bar . 2))
  (global-hl-line-mode t)
  (custom-set-faces
    '(trailing-whitespace ((t (:background "slategray3"))))
    '(ediff-current-diff-B ((t (:foreground "White" :background "green"))))
    '(ediff-current-diff-C ((t (:foreground "White" :background "green"))))))

;; ;; default font
(defun pye/load-font ()
  (interactive)
  (cond
    ((window-p) (set-frame-font "Iosevka SS12 Extended-12"))
    ((linux-p) (set-frame-font "Iosevka SS12 Extended-12"))
    (t (set-frame-font "Iosevka SS12 Extended-14"))))

(pye/load-font)


(setq prettify-symbols-unprettify-at-point t)
(global-prettify-symbols-mode t)

(use-package highlight-indent-guides
  :defer t
  :config
  (setq highlight-indent-guides-method 'character)
  (defun dont-highlight-first-level (level responsive display)
    (if (> 1 level) ; replace `1' with the number of guides you want to hide
        nil
      (highlight-indent-guides--highlighter-default level responsive display)))

  (setq highlight-indent-guides-highlighter-function 'dont-highlight-first-level))


(use-package rainbow-delimiters
  :defer t
  :config
  ;; enable rainbow delimiters in all programming-related modes
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; maximize
;; (custom-set-variables
;;  '(initial-frame-alist (quote ((fullscreen . maximized)))))

;; full path on title barf
(setq-default
  frame-title-format
  (list '((buffer-file-name " %f" (dired-directory
                                    dired-directory
                                    (revert-buffer-function " %b"
                                      ("%b - Dir:  " default-directory)))))))

(setq-default
  icon-title-format
  (list '((buffer-file-name " %f" (dired-directory
                                  dired-directory
                                  (revert-buffer-function " %b"
                                                          ("%b - Dir:  " default-directory)))))))

;; flash the screen on error; dont beep
(setq-default visible-bell t)

;;; make scratch as startup page
(setq inhibit-startup-message t)

;; Make show-trailing-whitespace default  ;;;;

(setq-default show-trailing-whitespace t)
(add-hook 'minibuffer-setup-hook
  (lambda () (setq-local show-trailing-whitespace nil)))

(add-hook 'eshell-mode-hook
  (lambda ()
    (setq show-trailing-whitespace nil)
    (setq buffer-face-mode-face `(:background "#56656b"))
    (buffer-face-mode 1)))

(add-hook 'eat-mode-hook
  (lambda ()
    (setq show-trailing-whitespace nil)))

;; make scratch as startup page
;; (setq initial-scratch-message "")

;; hide toolbar
(tool-bar-mode -1)

;; hide menu
(menu-bar-mode -1)

;; disable scrollbar
(cond
  ((boundp 'scroll-bar-mode)
    (scroll-bar-mode -1)))

;; modeline
(use-package doom-modeline
  :ensure t
  :init
  (setq doom-modeline-height 10)
  (setq doom-modeline-lsp t)
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-buffer-file-name-style 'truncate-except-project)
  (setq doom-modeline-vcs-max-length 20)
  (set-face-attribute 'mode-line nil :family "Iosevka SS12 Extended" :height 120)
  (set-face-attribute 'mode-line-inactive nil :family "Iosevka SS12 Extended" :height 100)
  (doom-modeline-mode t))

(use-package all-the-icons
  :if (display-graphic-p))

;; subtly flash the modeline for alert
(setq ring-bell-function
  (lambda ()
    (let ((orig-fg (face-foreground 'mode-line)))
      (set-face-foreground 'mode-line "#F2804F")
      (run-with-idle-timer 0.1 nil
        (lambda (fg) (set-face-foreground 'mode-line fg))
        orig-fg))))

;; (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
;; (add-to-list 'default-frame-alist '(ns-appearance . dark))

(use-package zone
  :config
  (zone-when-idle 120)
  (setq zone-programs [zone-pgm-drip-fretfully])

  (defun zone-choose (pgm)
    "Choose a PGM to run for `zone'."
    (interactive
      (list
        (completing-read
          "Program: "
          (mapcar 'symbol-name zone-programs))))
    (let ((zone-programs (list (intern pgm))))
      (zone))))
