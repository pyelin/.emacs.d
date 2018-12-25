(use-package ample-theme
  :config
  (load-theme 'ample-flat t)
  ;; ;; international orange cursor and selection background color
  (setq-default cursor-type '(bar . 3))
  (global-hl-line-mode t)
  (set-face-background hl-line-face "gray25")
  (set-cursor-color "white")
  (set-face-attribute 'region nil :background "#C0362C"))

(use-package highlight-indent-guides
  :defer t
  :config
  (setq highlight-indent-guides-method 'character))

(use-package rainbow-delimiters
  :config
  ;; enable rainbow delimiters in all programming-related modes
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; Make show-trailing-whitespace default  ;;;;
(setq-default show-trailing-whitespace t)
(add-hook
 'eww-mode-hook
 (lambda() (setq show-trailing-whitespace nil))
 (setq shr-color-visible-luminance-min 60))

;; maximize
(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized)))))

;; show scratch buffer at startup
(setq inhibit-startup-message t)
(setq initial-scratch-message "")

;; auto indent
;; (define-key global-map (kbd "RET") 'newline-and-indent)

;; hide toolbar
(tool-bar-mode -1)

;; hide menu
(menu-bar-mode -1)

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
;; (setq-default visible-bell t)

;; default font
(setq default-frame-alist '((font . "DejaVu Sans Mono-12")))

;; disable scrollbar
(scroll-bar-mode -1)

;; mode line
(setq-default
 mode-line-format
 '(; Position, including warning for 80 columns
   (:propertize "%4l:" face mode-line-position-face)
   (:eval (propertize "%3c" 'face
                      (if (>= (current-column) 80)
                          'mode-line-80col-face
                        'mode-line-position-face)))
   " "
   ; emacsclient [default -- keep?]
   mode-line-client
   " "
   ; read-only or modified status
   (:eval
    (cond (buffer-read-only
           (propertize " RO " 'face 'mode-line-read-only-face))
          ((buffer-modified-p)
           (propertize " * " 'face 'mode-line-modified-face))
          ))
   " "
   ; directory and buffer/file name
   (:propertize "%b" face mode-line-filename-face)
   ; narrow [default -- keep?]
   " %n"
   ; mode indicators: vc, recursive edit, major mode, minor modes, process, global
   (vc-mode vc-mode)
   " %["
   (:propertize mode-name
                face mode-line-mode-face)
   "%] "
   (:eval (propertize (format-mode-line minor-mode-alist)
                      'face 'mode-line-minor-mode-face))
   (:propertize mode-line-process
                face mode-line-process-face)
   (global-mode-string global-mode-string)
   " %p "
   (:eval 'mode-line-misc-info)
   )
)

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
                    :height 90
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

;; subtly flash the modeline for alert
(setq ring-bell-function
      (lambda ()
        (let ((orig-fg (face-foreground 'mode-line)))
          (set-face-foreground 'mode-line "#F2804F")
          (run-with-idle-timer 0.1 nil
                               (lambda (fg) (set-face-foreground 'mode-line fg))
                               orig-fg))))

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
