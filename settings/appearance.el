;;; package --- All configs related to appearance
;;; Code:
;;; Commentary:

(use-package nord-theme
  :config
  (load-theme 'nord t)
  ;; ;; international orange cursor and selection background color
  (setq-default cursor-type '(bar . 2))
  (global-hl-line-mode t)
  (custom-set-faces
    '(trailing-whitespace ((t (:background "slategray3"))))
    '(ediff-current-diff-B ((t (:foreground "White" :background "green"))))
    '(ediff-current-diff-C ((t (:foreground "White" :background "green"))))))

;; ;; default font
(cond
  ((window-p) (set-frame-font "Iosevka SS12 Extended-10"))
  (t (set-frame-font "Iosevka SS12 Extended-14")))

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

(add-hook 'eshell-mode-hook
  (lambda () (setq show-trailing-whitespace nil)))

;; make scratch as startup page
;; (setq initial-scratch-message "")

;; hide toolbar
(tool-bar-mode -1)

;; hide menu
(menu-bar-mode -1)

;; disable scrollbar
(scroll-bar-mode -1)

;; Extra mode line faces
(make-face 'mode-line-position-face)
(make-face 'mode-line-read-only-face)
(make-face 'mode-line-modified-face)
(make-face 'mode-line-mode-face)
(make-face 'mode-line-minor-mode-face)
(make-face 'mode-line-process-face)
(make-face 'mode-line-80col-face)
(make-face 'mode-line-filename-face)

(set-face-attribute 'mode-line nil
  :foreground "white" :background "black"
  :height 100
  :box nil)
(set-face-attribute 'mode-line-inactive nil
  :inherit 'mode-line
  :foreground "gray40" :background "gray20"
  :box nil)
(set-face-attribute 'mode-line-position-face nil
  :inherit 'mode-line-face)
(set-face-attribute 'mode-line-read-only-face nil
  :inherit 'mode-line-face
  :foreground "#4271ae")
(set-face-attribute 'mode-line-modified-face nil
  :inherit 'mode-line-face
  :foreground "#c82829")
(set-face-attribute 'mode-line-mode-face nil
  :inherit 'mode-line-face
  :foreground "gray80")
(set-face-attribute 'mode-line-minor-mode-face nil
  :inherit 'mode-line-mode-face
  :foreground "gray40")
(set-face-attribute 'mode-line-process-face nil
  :inherit 'mode-line-face
  :foreground "#718c00")
(set-face-attribute 'mode-line-80col-face nil
  :inherit 'mode-line-position-face
  :foreground "black" :background "#eab700")

;; mode line
(setq-default
  mode-line-format
  '(
     ; Position, including warning for 80 columns
     (:eval (format-mode-line "%4l:" 'mode-line-position-face))
     (:eval (format-mode-line "%2c"
              (if (>= (current-column) 80) 'mode-line-80col-face 'mode-line-position-face)))
     " "
     ; read-only or modified status
     (:eval
       (cond
         (buffer-read-only (format-mode-line "RO" 'mode-line-read-only-face))
         ((buffer-modified-p) (format-mode-line "*" 'mode-line-modified-face))
       )
     )
     " "
     (:eval (format-mode-line "%b" 'mode-line-filename-face))
     " "
     ; mode indicators: vc, recursive edit, major mode, minor modes, process, global
     (:eval (format-mode-line vc-mode))
     (:eval (format-mode-line (format " [%s]" mode-name) 'mode-line-mode-face))
     (:eval (format-mode-line minor-mode-alist 'mode-line-minor-mode-face))
     (:eval (format-mode-line mode-line-process 'mode-line-process-face))
     (:eval (format-mode-line global-mode-string))
     " "
     (:eval (format-mode-line "%p" 'mode-line-filename-face))
)

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
