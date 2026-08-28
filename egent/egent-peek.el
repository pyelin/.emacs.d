;;; egent-peek.el --- Posframe session switcher for egent  -*- lexical-binding: t -*-

;; Copyright (C) 2026

;; Author: Sreenivas Venkobarao
;; Package-Requires: ((emacs "29.1") (agent-shell "0.66.1") (posframe "1.4"))

;;; Commentary:

;; A transient posframe listing live sessions.  Navigating previews each
;; one in the window behind it; quitting puts back what was there.
;;
;; Unlike the sidebar this is deliberately live-only: it is meant to be
;; opened and dismissed in a second, and asking agents for their history
;; would make that wait on subprocesses.

;;; Code:

(require 'agent-shell)
(require 'egent-core)
(require 'posframe)

;;;; Customization

(defcustom egent-peek-position 'right
  "Edge of the frame where the peek posframe is anchored.
One of `top', `bottom', `left', `right'."
  :type '(choice (const top) (const bottom) (const left) (const right))
  :group 'egent)

(defcustom egent-peek-width 52
  "Width of the peek posframe in columns."
  :type 'integer
  :group 'egent)

(defcustom egent-peek-height 60
  "Maximum height of the peek posframe in rows."
  :type 'integer
  :group 'egent)

;;;; Internal state

(defconst egent-peek--buffer-name " *egent-peek*")

(defvar egent-peek--entries nil
  "Flat list of selectable entries.  Each element is a plist (:buffer BUF).")

(defvar egent-peek--current-idx 0
  "Index of the highlighted entry.")

(defvar egent-peek--origin-window nil
  "Window that was selected when peek was invoked.")

(defvar egent-peek--origin-buffer nil
  "Buffer shown in the origin window when peek was invoked.")

(defvar egent-peek--saved-terminal-map nil
  "Saved `overriding-terminal-local-map', restored when peek is dismissed.")

;; Only C-g is overridden, so editing in the parent frame is unaffected.
;; `overriding-terminal-local-map' is consulted before the child frame's
;; own keymaps, which makes C-g work whichever frame has focus.
(defvar egent-peek--quit-override-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-g") #'egent-peek-quit)
    map)
  "Terminal-wide override map active while the posframe is shown.")

;;;; Keymap

(defvar egent-peek-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map (kbd "n")   #'egent-peek-next)
    (define-key map (kbd "j")   #'egent-peek-next)
    (define-key map (kbd "p")   #'egent-peek-prev)
    (define-key map (kbd "k")   #'egent-peek-prev)
    (define-key map (kbd "RET") #'egent-peek-select)
    (define-key map (kbd "g")   #'egent-peek-quit)
    (define-key map (kbd "q")   #'egent-peek-quit)
    (define-key map (kbd "C-g") #'egent-peek-quit)
    (define-key map (kbd "s")   #'egent-peek-new-shell)
    map)
  "Keymap active inside the peek posframe.")

;;;; Helpers

(defun egent-peek--clear-override ()
  "Restore `overriding-terminal-local-map' to its pre-peek value."
  (when (eq overriding-terminal-local-map egent-peek--quit-override-map)
    (setq overriding-terminal-local-map egent-peek--saved-terminal-map))
  (setq egent-peek--saved-terminal-map nil))

(defun egent-peek--dismiss ()
  "Delete the posframe and reset entry state."
  (egent-peek--clear-override)
  (posframe-delete egent-peek--buffer-name)
  (when-let* ((buf (get-buffer egent-peek--buffer-name)))
    (kill-buffer buf))
  (setq egent-peek--entries nil
        egent-peek--current-idx 0))

(defun egent-peek--preview ()
  "Show the highlighted session in the origin window."
  (when-let* ((entry (nth egent-peek--current-idx egent-peek--entries))
              (shell-buf (plist-get entry :buffer))
              (disp-buf (egent-preferred-buffer shell-buf)))
    (when (and (window-live-p egent-peek--origin-window)
               (buffer-live-p disp-buf))
      (set-window-buffer egent-peek--origin-window disp-buf))))

;;;; Rendering

(defun egent-peek--render (groups)
  "Render GROUPS into the peek buffer."
  (with-current-buffer (get-buffer-create egent-peek--buffer-name)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (setq egent-peek--entries nil)
      (insert "\n")
      (dolist (group groups)
        (let ((pname (cadr group))
              (bufs (caddr group)))
          (insert (propertize (concat "    " pname "\n")
                              'face 'egent-project))
          (dolist (buf bufs)
            (push (list :buffer buf) egent-peek--entries)
            (insert (propertize (concat "      " (egent-icon (egent-buffer-state buf))
                                        " " (egent-buffer-row-label
                                             buf (max 1 (- egent-peek-width 8)))
                                        "\n")
                                'egent-peek-buffer buf)))
          (insert "\n")))
      (insert (propertize "    n/p navigate   RET select   q quit\n" 'face 'shadow))
      (insert "\n")
      (setq egent-peek--entries (nreverse egent-peek--entries))
      (setq buffer-read-only t))
    (goto-char (point-min))))

(defvar egent-peek--highlight-overlay nil
  "Overlay marking the selected line.")

(defun egent-peek--highlight (idx)
  "Highlight the entry at IDX."
  (with-current-buffer (get-buffer-create egent-peek--buffer-name)
    (let ((inhibit-read-only t))
      (unless (overlayp egent-peek--highlight-overlay)
        (setq egent-peek--highlight-overlay (make-overlay (point-min) (point-min))))
      (let ((ov egent-peek--highlight-overlay))
        (move-overlay ov (point-min) (point-min))
        (when-let* ((entry (nth idx egent-peek--entries))
                    (buf (plist-get entry :buffer))
                    (pos (text-property-any (point-min) (point-max)
                                            'egent-peek-buffer buf)))
          (move-overlay ov pos
                        (min (1+ (save-excursion (goto-char pos) (line-end-position)))
                             (point-max)))
          (overlay-put ov 'face 'highlight))))))

(defun egent-peek--poshandler (info)
  "Anchor the posframe according to `egent-peek-position' using INFO."
  (let* ((fw (plist-get info :parent-frame-width))
         (fh (plist-get info :parent-frame-height))
         (pw (plist-get info :posframe-width))
         (ph (plist-get info :posframe-height))
         (pad 8))
    (pcase egent-peek-position
      ('right  (cons (- fw pw pad) pad))
      ('left   (cons pad pad))
      ('top    (cons (/ (- fw pw) 2) pad))
      ('bottom (cons (/ (- fw pw) 2) (- fh ph pad))))))

;;;; Commands

(defun egent-peek-next ()
  "Move to the next entry and preview it."
  (interactive)
  (when egent-peek--entries
    (setq egent-peek--current-idx
          (mod (1+ egent-peek--current-idx) (length egent-peek--entries)))
    (egent-peek--highlight egent-peek--current-idx)
    (egent-peek--preview)))

(defun egent-peek-prev ()
  "Move to the previous entry and preview it."
  (interactive)
  (when egent-peek--entries
    (setq egent-peek--current-idx
          (mod (1- egent-peek--current-idx) (length egent-peek--entries)))
    (egent-peek--highlight egent-peek--current-idx)
    (egent-peek--preview)))

(defun egent-peek-select ()
  "Switch to the highlighted session and dismiss the posframe."
  (interactive)
  (when-let* ((entry (nth egent-peek--current-idx egent-peek--entries))
              (shell-buf (plist-get entry :buffer))
              (disp-buf (egent-preferred-buffer shell-buf))
              (win egent-peek--origin-window))
    (egent-peek--dismiss)
    (when (and (window-live-p win) (buffer-live-p disp-buf))
      (select-window win)
      (switch-to-buffer disp-buf))))

(defun egent-peek-quit ()
  "Dismiss the posframe and restore the original buffer."
  (interactive)
  (let ((win egent-peek--origin-window)
        (orig egent-peek--origin-buffer))
    (egent-peek--dismiss)
    (setq egent-peek--origin-buffer nil)
    (when (window-live-p win)
      (select-window win)
      (when (and (buffer-live-p orig) (not (eq (window-buffer win) orig)))
        (set-window-buffer win orig)))))

(defun egent-peek-new-shell ()
  "Dismiss peek and start a new shell in the origin window's project."
  (interactive)
  (let ((win egent-peek--origin-window))
    (egent-peek-quit)
    (when (window-live-p win)
      (select-window win)
      (agent-shell-new-shell))))

;;;; Entry point

;;;###autoload
(defun egent-peek ()
  "Show a posframe listing live agent-shell sessions grouped by project.

n/p navigates, RET selects, q or C-g quits."
  (interactive)
  (let ((origin-win (selected-window))
        (groups (egent-grouped-buffers)))
    (unless groups
      (user-error "No agent-shell buffers found"))
    (setq egent-peek--origin-window origin-win
          egent-peek--origin-buffer (window-buffer origin-win)
          egent-peek--current-idx 0)
    (egent-peek--render groups)
    (egent-peek--highlight 0)
    (with-current-buffer egent-peek--buffer-name
      (use-local-map egent-peek-map))
    (setq egent-peek--saved-terminal-map overriding-terminal-local-map
          overriding-terminal-local-map egent-peek--quit-override-map)
    (posframe-show egent-peek--buffer-name
                   :poshandler #'egent-peek--poshandler
                   :width egent-peek-width
                   :max-height egent-peek-height
                   :internal-border-width 4
                   :border-color (face-foreground 'shadow nil t)
                   :accept-focus t)
    (egent-peek--preview)
    (let ((frame (buffer-local-value 'posframe--frame
                                     (get-buffer egent-peek--buffer-name))))
      (when (framep frame)
        (select-frame-set-input-focus frame)
        (select-window (frame-selected-window frame) t)))))

(provide 'egent-peek)
;;; egent-peek.el ends here
