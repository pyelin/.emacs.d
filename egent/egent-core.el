;;; egent-core.el --- Shared state and rendering helpers for egent  -*- lexical-binding: t -*-

;; Copyright (C) 2026

;; Author: Sreenivas Venkobarao
;; Package-Requires: ((emacs "29.1") (agent-shell "0.66.1"))

;;; Commentary:

;; Grouping, status icons and buffer introspection shared by every egent
;; surface.  These live here rather than inside whichever surface happened
;; to need them first, so the sidebar does not have to load the posframe
;; code just to group buffers by project.

;;; Code:

(require 'agent-shell)
(require 'agent-shell-viewport)
(require 'cl-lib)
(require 'map)
(require 'seq)

;; Loaded by `egent', but egent-session requires this file, so the fallback
;; is reached through `fboundp' rather than a `require'.
(declare-function egent-session-cached-title "egent-session" (session-id))

;;;; Customization

(defgroup egent nil
  "Session heads-up display for `agent-shell'."
  :group 'agent-shell
  :prefix "egent-")

(defcustom egent-fallback-icons
  '((idle . ("✓" . success))
    (busy . ("◔" . warning))
    (dead . ("✗" . error))
    (past . ("↺" . shadow)))
  "Unicode fallbacks used when image display is unavailable.
Each entry is (STATE . (CHAR . FACE)).  `font-lock-face' is used so the
colour survives the outer `face' properties applied while rendering."
  :type '(alist :key-type symbol :value-type (cons string face))
  :group 'egent)

;;;; Faces

(defface egent-project
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for project group headers."
  :group 'egent)

(defface egent-session
  '((t :inherit shadow))
  "Face for resumable (not currently open) sessions."
  :group 'egent)

(defface egent-session-time
  '((t :inherit font-lock-comment-face))
  "Face for the relative timestamp shown next to a resumable session."
  :group 'egent)

(defface egent-buffer-name
  '((t :inherit shadow))
  "Face for the buffer name shown beside a session's name."
  :group 'egent)

;;;; Status icons

(defvar egent--icon-cache nil
  "Alist of (STATE . IMAGE) built lazily on first use.")

(defconst egent--icon-svgs
  '((idle . "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"20\" height=\"20\" viewBox=\"0 0 20 20\">
  <circle cx=\"10\" cy=\"10\" r=\"8.5\" fill=\"#4E9A72\"/>
  <polyline points=\"4.5,10.5 8.5,14.5 16,5.5\"
            stroke=\"white\" stroke-width=\"2.5\" fill=\"none\"
            stroke-linecap=\"round\" stroke-linejoin=\"round\"/>
</svg>")
    (busy . "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"20\" height=\"20\" viewBox=\"0 0 20 20\">
  <polygon points=\"10,2 18.5,17.5 1.5,17.5\" fill=\"#C9922A\"/>
</svg>")
    (dead . "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"20\" height=\"20\" viewBox=\"0 0 20 20\">
  <circle cx=\"10\" cy=\"10\" r=\"8.5\" fill=\"#C0392B\"/>
  <line x1=\"6.5\" y1=\"6.5\" x2=\"13.5\" y2=\"13.5\" stroke=\"white\" stroke-width=\"2.5\" stroke-linecap=\"round\"/>
  <line x1=\"13.5\" y1=\"6.5\" x2=\"6.5\" y2=\"13.5\" stroke=\"white\" stroke-width=\"2.5\" stroke-linecap=\"round\"/>
</svg>")
    (past . "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"20\" height=\"20\" viewBox=\"0 0 20 20\">
  <circle cx=\"10\" cy=\"10\" r=\"7.5\" fill=\"none\" stroke=\"#8A8A8A\" stroke-width=\"2\"
          stroke-dasharray=\"3 2.5\"/>
</svg>"))
  "Inline SVG strings for each session state.")

(defun egent-icon (state)
  "Return a propertized string displaying the icon for STATE."
  (if (and (display-graphic-p) (image-type-available-p 'svg))
      (progn
        (unless egent--icon-cache
          (setq egent--icon-cache
                (mapcar (lambda (pair)
                          (cons (car pair)
                                (create-image (cdr pair) 'svg t :ascent 'center)))
                        egent--icon-svgs)))
        (propertize " " 'display (alist-get state egent--icon-cache)))
    (let ((fallback (alist-get state egent-fallback-icons)))
      (propertize (car fallback) 'font-lock-face (cdr fallback)))))

;;;; Buffer introspection

(defun egent-buffer-state (buf)
  "Return `busy', `idle', or `dead' for BUF."
  (if (buffer-live-p buf)
      (with-current-buffer buf
        (if (shell-maker-busy) 'busy 'idle))
    'dead))

(defun egent-preferred-buffer (shell-buf)
  "Return the best buffer to display for SHELL-BUF.
Prefers an existing viewport buffer so its mode (view or edit) survives
the switch, falling back to the shell buffer itself."
  (or (ignore-errors
        (agent-shell-viewport--buffer :shell-buffer shell-buf :existing-only t))
      shell-buf))

(defun egent--buffer-state-value (buf key)
  "Return KEY from BUF's `agent-shell--state', or nil.
KEY is a list of keys walked with `map-nested-elt'."
  (when (and (buffer-live-p buf)
             (buffer-local-boundp 'agent-shell--state buf))
    (map-nested-elt (buffer-local-value 'agent-shell--state buf) key)))

(defun egent-buffer-session-id (buf)
  "Return the ACP session id BUF is attached to, or nil.
Used to hide sessions that are already open from the resumable list."
  (egent--buffer-state-value buf '(:session :id)))

(defun egent-buffer-agent-identifier (buf)
  "Return the agent identifier symbol BUF was started with, or nil."
  (egent--buffer-state-value buf '(:agent-config :identifier)))

(defun egent-buffer-config (buf)
  "Return the agent config alist BUF was started with, or nil."
  (egent--buffer-state-value buf '(:agent-config)))

(defun egent-buffer-session-title (buf)
  "Return the title BUF's session reports, or nil.
`agent-shell' seeds this from the first prompt and refreshes it from the
agent as the conversation grows, so it is empty until something is sent."
  (egent--buffer-state-value buf '(:session :title)))

(defvar-local egent--session-name nil
  "Name given to this session through egent, or nil.
Held apart from the buffer name so renaming a session does not detach it
from the name `switch-to-buffer' knows it by, and apart from the agent's
own title, which the agent rewrites as the conversation grows.")
(put 'egent--session-name 'permanent-local t)

(defun egent-buffer-session-name (buf)
  "Return the name given to BUF through egent, or nil."
  (when (buffer-live-p buf)
    (buffer-local-value 'egent--session-name buf)))

(defun egent-set-buffer-session-name (buf name)
  "Name the session BUF NAME, or clear the name when NAME is empty."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (setq-local egent--session-name
                  (egent-nonempty (egent-one-line name))))))

(defun egent-buffer-label (buf)
  "Return the display label for the live session BUF.
Prefers a name given through egent, then the session title, then the
title a fetch cached for it, over the buffer name, which only
distinguishes sessions by the order they were opened in."
  (or (egent-nonempty (egent-buffer-session-name buf))
      (egent-nonempty (egent-one-line (egent-buffer-session-title buf)))
      (and (fboundp 'egent-session-cached-title)
           (egent-session-cached-title (egent-buffer-session-id buf)))
      (buffer-name buf)))

(defun egent-buffer-row-label (buf &optional width)
  "Return BUF's label as \"NAME (BUFFER)\", fitted into WIDTH columns.
The buffer name rides along so a session named something else can still
be found with `switch-to-buffer'.  The name is served first and the
buffer name gets what is left, since a session is picked by what it is
about; it is dropped entirely when nothing is left for it, and when the
session has no name of its own to distinguish it from."
  (let* ((name (egent-buffer-label buf))
         (bname (buffer-name buf))
         (suffix (lambda (text)
                   (propertize (concat " (" text ")")
                               'face 'egent-buffer-name))))
    (cond
     ((equal name bname) (egent-truncate name width))
     ((null width) (concat name (funcall suffix bname)))
     (t
      ;; Four columns is the narrowest " (x)" worth appending.
      (let* ((shown (egent-truncate name (- width 4)))
             (room (- width (string-width shown) 3)))
        (if (< room 2)
            (egent-truncate name width)
          (concat shown (funcall suffix (egent-truncate bname room)))))))))

;;;; Text helpers

(defun egent-one-line (text)
  "Return TEXT collapsed onto one line, or nil when TEXT is not a string.
A title seeded from a prompt can carry newlines and runs of spaces, which
would otherwise break a single-line row."
  (when (stringp text)
    (string-join (split-string text nil t) " ")))

(defun egent-nonempty (text)
  "Return TEXT unless it is nil or empty."
  (unless (or (null text) (string-empty-p text))
    text))

(defun egent-truncate (text width)
  "Return TEXT shortened to WIDTH columns, ellipsized when it does not fit."
  (if (or (null width) (<= (string-width text) width))
      text
    (truncate-string-to-width text (max 1 width) nil nil t)))

;;;; Agent configs

(defun egent-agent-configs ()
  "Return every resolved agent config known to `agent-shell'."
  (agent-shell--resolved-agent-configs))

(defun egent-config-by-identifier (identifier)
  "Return the resolved agent config whose `:identifier' is IDENTIFIER."
  (seq-find (lambda (config)
              (eq (map-elt config :identifier) identifier))
            (egent-agent-configs)))

(defun egent-config-name (config)
  "Return a human readable name for CONFIG."
  (or (map-elt config :mode-line-name)
      (map-elt config :buffer-name)
      (format "%s" (map-elt config :identifier))))

;;;; Grouping

(defun egent-grouped-buffers ()
  "Return a list of (ROOT PROJECT-NAME BUFFERS) sorted by project name.
Buffers inside a group are sorted by name so the list does not reshuffle
as buffers are visited."
  (let ((table (make-hash-table :test 'equal))
        (order nil))
    (dolist (buf (agent-shell-buffers))
      (let ((root  (with-current-buffer buf (agent-shell-cwd)))
            (pname (with-current-buffer buf (agent-shell--project-name))))
        (unless (gethash root table)
          (puthash root (list pname nil) table)
          (push root order))
        (let ((entry (gethash root table)))
          (setcar (cdr entry) (append (cadr entry) (list buf))))))
    (let ((groups (mapcar (lambda (root)
                            (let ((e (gethash root table)))
                              (list root (car e)
                                    (sort (copy-sequence (cadr e))
                                          (lambda (a b)
                                            (string< (buffer-name a)
                                                     (buffer-name b)))))))
                          (nreverse order))))
      (sort groups (lambda (a b) (string< (cadr a) (cadr b)))))))

;;;; Time formatting

(defun egent-relative-time (iso)
  "Return a compact relative description of ISO, an ISO-8601 string.
Returns an empty string when ISO is nil or unparseable, since a session
list is still useful without timestamps."
  (or (ignore-errors
        (when (and iso (stringp iso))
          (let* ((then (float-time (date-to-time iso)))
                 (secs (max 0 (- (float-time) then))))
            (cond
             ((< secs 60)    "just now")
             ((< secs 3600)  (format "%dm ago" (floor secs 60)))
             ((< secs 86400) (format "%dh ago" (floor secs 3600)))
             ((< secs 604800) (format "%dd ago" (floor secs 86400)))
             (t (format-time-string "%b %e" then))))))
      ""))

(provide 'egent-core)
;;; egent-core.el ends here
