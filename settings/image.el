;;; image.el --- Viewing images in a terminal frame -*- lexical-binding: t; -*-

;;; Commentary:

;; This Emacs runs headless over SSH: its frames are terminal frames and
;; the build carries no image support at all (`image-types' is unbound),
;; so anything that would draw an image -- `image-mode', agent-shell's
;; inline previews -- gives back raw bytes or a bare link.  chafa renders
;; the file to coloured text instead, which a terminal frame can show.
;;
;; Nothing here takes over when images can be drawn, so the same config
;; still opens images normally on a graphical Emacs.

;;; Code:

(require 'ansi-color)

(defvar pye/image-preview-program "chafa"
  "Program rendering an image as coloured terminal text.")

(defvar pye/image-preview-args
  '("--format=symbols" "--polite=on" "--animate=off")
  "Arguments always passed to `pye/image-preview-program'.
Polite mode keeps chafa from emitting cursor and screen sequences,
leaving only the colour codes `ansi-color' turns into faces.")

(defconst pye/image-file-regexp
  "\\.\\(?:png\\|jpe?g\\|gif\\|webp\\|bmp\\|tiff?\\|svg\\)\\'"
  "Files `pye/find-file-image' treats as images.")

(defvar-local pye/image-preview--file nil
  "Image this buffer is showing.
Held here rather than read from `buffer-file-name', which is cleared so
the rendering cannot be saved back over the image.")

(defvar-local pye/image-preview--size nil
  "Window size the buffer was last rendered for, as (COLUMNS . ROWS).")

(defun pye/image-render-to-string (file columns rows &optional frame)
  "Return FILE drawn as coloured text COLUMNS wide and ROWS tall.
Returns nil when the renderer is missing or draws nothing, so callers
can fall back to whatever they show without an image.  FRAME decides how
many colours to ask for, defaulting to the selected one."
  (when (and (executable-find pye/image-preview-program)
             (file-readable-p file))
    (with-temp-buffer
      (apply #'call-process pye/image-preview-program nil t nil
             (append pye/image-preview-args
                     (list (format "--colors=%d"
                                   (if (>= (display-color-cells frame) 256)
                                       256
                                     16))
                           (format "--size=%dx%d" columns rows)
                           (file-local-name (expand-file-name file)))))
      ;; `ansi-color' colours with overlays by default, which would pile
      ;; up empty across re-renders, and its text-property alternative
      ;; sets `font-lock-face', which only paints where font-lock runs.
      (let ((ansi-color-apply-face-function
             (lambda (start end face)
               (when face
                 (put-text-property start end 'face face)))))
        (ansi-color-apply-on-region (point-min) (point-max)))
      (unless (zerop (buffer-size))
        (string-trim-right (buffer-string) "\n+")))))

(defun pye/image-preview--render (window)
  "Draw this buffer's image to fill WINDOW."
  (let* ((inhibit-read-only t)
         ;; A column short of the window: chafa pads its last cell, and a
         ;; full-width line wraps into a blank one.
         (text (pye/image-render-to-string
                pye/image-preview--file
                (max 20 (1- (window-body-width window)))
                (max 10 (1- (window-body-height window)))
                (window-frame window))))
    (erase-buffer)
    (insert (or text
                (format "%s is not installed, so %s cannot be shown here."
                        pye/image-preview-program
                        (file-name-nondirectory pye/image-preview--file))))
    (set-buffer-modified-p nil)
    (goto-char (point-min))))

(defun pye/image-preview--refresh ()
  "Redraw the image when the window showing it changed size.
The rendering is made of characters, so it is tied to the window it was
drawn for and has to be redone when that window is resized."
  (when-let* ((window (get-buffer-window (current-buffer)))
              (size (cons (window-body-width window)
                          (window-body-height window)))
              ((not (equal size pye/image-preview--size))))
    (setq pye/image-preview--size size)
    (pye/image-preview--render window)))

(define-derived-mode pye/image-preview-mode special-mode "Image Preview"
  "Show an image as coloured text, for frames that cannot draw one.

The buffer stops visiting the file, so the rendering that replaces its
contents can never be written back over the image."
  (let ((file (buffer-file-name)))
    (when file
      (set-visited-file-name nil 'no-query)
      (setq pye/image-preview--file file)))
  (setq-local truncate-lines t)
  (setq-local revert-buffer-function
              (lambda (&rest _)
                (setq pye/image-preview--size nil)
                (pye/image-preview--refresh)))
  ;; Renders on the first size change this hook sees, which is the buffer
  ;; being displayed: there is no window to size the image to before that.
  (add-hook 'window-configuration-change-hook
            #'pye/image-preview--refresh nil 'local))

;;;###autoload
(defun pye/image-preview (file)
  "Show FILE as coloured text, returning the window showing it."
  (interactive "fImage: ")
  (let ((buffer (get-buffer-create
                 (format "*image: %s*" (file-name-nondirectory file)))))
    (with-current-buffer buffer
      (pye/image-preview-mode)
      (setq pye/image-preview--file (expand-file-name file)))
    (let ((window (display-buffer buffer)))
      ;; Drawn here rather than left to the hook below: showing a buffer
      ;; that is already on screen changes no window configuration.
      (when window
        (with-current-buffer buffer
          (pye/image-preview--refresh)))
      window)))

(defun pye/find-file-image ()
  "Open the visited image, drawing it where the frame can and text where not."
  (if (and (display-images-p)
           (image-supported-file-p (buffer-file-name)))
      (image-mode)
    (pye/image-preview-mode)))

;; Ahead of `image-mode''s own entry, which this defers to when the frame
;; can draw the image.
(add-to-list 'auto-mode-alist (cons pye/image-file-regexp #'pye/find-file-image))

(provide 'image-settings)
;;; image.el ends here
