;;; egent-sidebar.el --- Session sidebar workspace for egent  -*- lexical-binding: t -*-

;; Copyright (C) 2026

;; Author: Sreenivas Venkobarao
;; Package-Requires: ((emacs "29.1") (agent-shell "0.66.1"))

;;; Commentary:

;; A sidebar listing every agent-shell session grouped by project: the
;; ones open in a buffer, and — once fetched — the ones the agent still
;; remembers but Emacs has forgotten.  Selecting the latter resumes it.
;;
;; `persp-mode' is used when available but is not required; without it the
;; window configuration is saved and restored instead.

;;; Code:

(require 'agent-shell)
(require 'agent-shell-viewport)
(require 'cl-lib)
(require 'egent-core)
(require 'egent-session-name)
(require 'egent-session)
(require 'map)
(require 'seq)

;; persp-mode is optional; these are only called after `featurep' confirms it.
(declare-function persp-mode "persp-mode" (&optional arg))
(declare-function persp-switch "persp-mode" (name &optional frame window))
(declare-function persp-buffers "persp-mode" (persp))
(declare-function persp-add-buffer "persp-mode" (buff &optional persp switchorno noask-to-remove-from-other-persps))
(declare-function persp-remove-buffer "persp-mode" (buff &optional persp noswitch nohooks))
(declare-function get-current-persp "persp-mode" (&optional frame window))
(declare-function safe-persp-name "persp-mode" (p))

;;;; Customization

(defcustom egent-sidebar-width 50
  "Width of the sidebar in columns."
  :type 'integer
  :group 'egent)

(defcustom egent-sidebar-auto-fetch-sessions t
  "Whether opening the sidebar asks each project for its past sessions.
The fetch runs once per project per Emacs session and, with
`egent-session-agents' left at `auto', only queries agents already in use
there.  Set to nil to fetch only on demand."
  :type 'boolean
  :group 'egent)

(defcustom egent-sidebar-use-perspective 'auto
  "Whether the sidebar opens in a dedicated `persp-mode' perspective.
`auto' uses one when `persp-mode' is installed and falls back to saving
and restoring the window configuration."
  :type '(choice (const :tag "Use persp-mode when available" auto)
                 (const :tag "Never use persp-mode" never))
  :group 'egent)

(defcustom egent-sidebar-lock-perspective nil
  "When non-nil, restrict the perspective to agent-shell buffers.
Only meaningful when the sidebar runs inside a perspective.  While it is
active, file-finding commands are blocked so the workspace stays a
session switcher rather than turning into an ordinary project window."
  :type 'boolean
  :group 'egent)

;;;; Constants

(defconst egent-sidebar--persp-name "*egent*")
(defconst egent-sidebar--buffer-name " *egent-sidebar*")

(defconst egent-sidebar--hints
  '(((egent-sidebar-next egent-sidebar-prev) . "navigate")
    (egent-sidebar-select                    . "select/resume")
    (egent-sidebar-collapse                  . "collapse")
    (egent-sidebar-fetch-sessions            . "past sessions")
    (egent-sidebar-name-session              . "name session")
    (egent-sidebar-rename-session            . "rename")
    (egent-sidebar-kill                      . "kill/delete")
    (egent-sidebar-reload                    . "refresh")
    (egent-sidebar-new-shell                 . "new shell")
    (egent-sidebar-toggle                    . "quit"))
  "Command/description pairs shown in the sidebar's footer.")

;;;; Internal state

(defvar egent-sidebar--prev-persp nil
  "Perspective name to return to on exit.")

(defvar egent-sidebar--saved-wconf nil
  "Window configuration to restore on exit when not using perspectives.")

(defvar egent-sidebar--using-persp nil
  "Whether the open workspace entered through a perspective.
Recorded on entry so exit undoes what entry actually did, even if
`persp-mode' was loaded in between.")

;; Each entry is a plist with :type (`project', `buffer' or `session'),
;; :root ROOT and, depending on type, :buffer, :session and :config.
;; Project headers are navigable while collapsed so they can be reopened.
(defvar egent-sidebar--entries nil
  "Flat list of navigable entries backing the rendered sidebar.")

(defvar egent-sidebar--current-idx 0
  "Index of the highlighted entry.")

(defvar egent-sidebar--collapsed nil
  "List of project roots currently collapsed.")

(defvar egent-sidebar--main-window nil
  "The main content window in the sidebar workspace.")

(defvar egent-sidebar--refresh-timer nil
  "Repeating timer that re-renders when a session's busy state changes.")

(defvar egent-sidebar--state-snapshot nil
  "Alist of (BUFFER . STATE) captured at last render, used to detect changes.")

;;;; Faces

(defface egent-sidebar-selection
  '((((class color) (background dark))
     :background "#2d3b2d" :extend t)
    (((class color) (background light))
     :background "#d4e4d4" :extend t))
  "Face for the selected entry.
Intentionally dim — enough to show position without glare."
  :group 'egent)

(defface egent-sidebar-hint-key
  '((t :inherit font-lock-constant-face))
  "Face for key names in the footer."
  :group 'egent)

(defface egent-sidebar-hint-desc
  '((t :inherit shadow))
  "Face for descriptions in the footer."
  :group 'egent)

;;;; Keymap

(defvar egent-sidebar-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map (kbd "j")   #'egent-sidebar-next)
    (define-key map (kbd "n")   #'egent-sidebar-next)
    (define-key map (kbd "k")   #'egent-sidebar-prev)
    (define-key map (kbd "p")   #'egent-sidebar-prev)
    (define-key map (kbd "RET") #'egent-sidebar-select)
    (define-key map (kbd "TAB") #'egent-sidebar-collapse)
    (define-key map (kbd "S")   #'egent-sidebar-fetch-sessions)
    (define-key map (kbd "o")   #'egent-resume)
    (define-key map (kbd "r")   #'egent-sidebar-name-session)
    (define-key map (kbd "R")   #'egent-sidebar-name-all-sessions)
    (define-key map (kbd "M-r") #'egent-sidebar-rename-session)
    (define-key map (kbd "K")   #'egent-sidebar-kill)
    (define-key map (kbd "g")   #'egent-sidebar-reload)
    (define-key map (kbd "s")   #'egent-sidebar-new-shell)
    (define-key map (kbd "C-g") #'egent-sidebar-toggle)
    (define-key map (kbd "q")   #'egent-sidebar-toggle)
    (define-key map (kbd "<mouse-1>")        #'egent-sidebar-mouse-select)
    (define-key map (kbd "<double-mouse-1>") #'egent-sidebar-mouse-select-double)
    map)
  "Keymap for the egent sidebar.")

;;;; Perspective handling

(defun egent-sidebar--perspective-p ()
  "Return non-nil when the sidebar should use `persp-mode'.
`auto' loads `persp-mode' when it is installed, so having the package is
enough to get a dedicated workspace without configuring anything."
  (and (eq egent-sidebar-use-perspective 'auto)
       (or (featurep 'persp-mode)
           (require 'persp-mode nil 'noerror))
       t))

(defun egent-sidebar--active-p ()
  "Return non-nil when the sidebar workspace is currently open."
  (and (get-buffer egent-sidebar--buffer-name)
       (get-buffer-window egent-sidebar--buffer-name)
       t))

(defun egent-sidebar--in-locked-persp-p ()
  "Return non-nil when inside a locked egent perspective."
  (and egent-sidebar-lock-perspective
       egent-sidebar--using-persp
       (fboundp 'safe-persp-name)
       (fboundp 'get-current-persp)
       (string= (safe-persp-name (get-current-persp)) egent-sidebar--persp-name)))

(defun egent-sidebar--block-file-finder (&rest _)
  "Signal a `user-error' when a file finder runs inside the locked workspace."
  (when (egent-sidebar--in-locked-persp-p)
    (user-error "Quit the egent workspace (q) to visit files")))

(defun egent-sidebar--populate-perspective ()
  "Restrict the current perspective to agent-shell and sidebar buffers."
  (when (and egent-sidebar-lock-perspective
             egent-sidebar--using-persp
             (fboundp 'get-current-persp)
             (fboundp 'persp-buffers))
    (let* ((persp (get-current-persp))
           (keep (append (agent-shell-buffers)
                         (when-let* ((b (get-buffer egent-sidebar--buffer-name)))
                           (list b)))))
      (dolist (buf (copy-sequence (persp-buffers persp)))
        (unless (memq buf keep)
          (persp-remove-buffer buf persp t t)))
      (dolist (buf (agent-shell-buffers))
        (when (buffer-live-p buf)
          (persp-add-buffer buf persp nil t))))))

(defun egent-sidebar--enter-workspace ()
  "Switch into the egent workspace, remembering how to get back."
  (setq egent-sidebar--using-persp (egent-sidebar--perspective-p))
  (if egent-sidebar--using-persp
      (progn
        (unless (bound-and-true-p persp-mode)
          (persp-mode 1))
        (setq egent-sidebar--prev-persp (safe-persp-name (get-current-persp)))
        (persp-switch egent-sidebar--persp-name))
    (setq egent-sidebar--saved-wconf (current-window-configuration)))
  (when egent-sidebar-lock-perspective
    (advice-add 'projectile-find-file :before #'egent-sidebar--block-file-finder)
    (advice-add 'project-find-file    :before #'egent-sidebar--block-file-finder)))

(defun egent-sidebar--exit-workspace ()
  "Leave the egent workspace, restoring the previous layout."
  (advice-remove 'projectile-find-file #'egent-sidebar--block-file-finder)
  (advice-remove 'project-find-file    #'egent-sidebar--block-file-finder)
  (if egent-sidebar--using-persp
      (when egent-sidebar--prev-persp
        (persp-switch egent-sidebar--prev-persp))
    (when (window-configuration-p egent-sidebar--saved-wconf)
      (set-window-configuration egent-sidebar--saved-wconf)))
  ;; Cleared here rather than in `egent-sidebar--teardown', which runs first
  ;; and would otherwise discard the layout before it is restored.
  (setq egent-sidebar--using-persp nil
        egent-sidebar--prev-persp nil
        egent-sidebar--saved-wconf nil))

;;;; Auto refresh

(defun egent-sidebar--capture-states ()
  "Return an alist of (BUFFER . STATE) sorted by buffer name.
Sorting by name rather than keeping the MRU order of `agent-shell-buffers'
avoids re-rendering merely because a buffer was visited."
  (sort (mapcar (lambda (buf) (cons buf (egent-buffer-state buf)))
                (agent-shell-buffers))
        (lambda (a b) (string< (buffer-name (car a)) (buffer-name (car b))))))

(defun egent-sidebar--maybe-refresh ()
  "Re-render only when a session's state actually changed."
  (when (get-buffer egent-sidebar--buffer-name)
    (let ((current (egent-sidebar--capture-states)))
      (unless (equal current egent-sidebar--state-snapshot)
        (setq egent-sidebar--state-snapshot current)
        (let ((buf (egent-sidebar--current-buffer)))
          (egent-sidebar--render)
          (egent-sidebar--populate-perspective)
          (egent-sidebar--restore-idx buf)
          (egent-sidebar--highlight egent-sidebar--current-idx))))))

;;;; Cursor helpers

(defun egent-sidebar--entry ()
  "Return the highlighted entry plist, or nil."
  (nth egent-sidebar--current-idx egent-sidebar--entries))

(defun egent-sidebar--current-buffer ()
  "Return the buffer of the highlighted entry, or nil."
  (plist-get (egent-sidebar--entry) :buffer))

(defun egent-sidebar--current-root ()
  "Return the project root of the highlighted entry, or nil."
  (plist-get (egent-sidebar--entry) :root))

(defun egent-sidebar--restore-idx (buf)
  "Point the selection back at BUF after a re-render.
Falls back to clamping the previous index when BUF is gone."
  (setq egent-sidebar--current-idx
        (or (and (buffer-live-p buf)
                 (cl-position-if (lambda (e)
                                   (and (eq (plist-get e :type) 'buffer)
                                        (eq (plist-get e :buffer) buf)))
                                 egent-sidebar--entries))
            (min egent-sidebar--current-idx
                 (max 0 (1- (length egent-sidebar--entries)))))))

;;;; Rendering

(defun egent-sidebar--format-hint-key (cmd-or-cmds)
  "Return the key string bound to CMD-OR-CMDS in `egent-sidebar-map'."
  (let ((lookup (lambda (cmd)
                  (when-let* ((keys (where-is-internal cmd egent-sidebar-map)))
                    (when-let* ((key (seq-find
                                     (lambda (k)
                                       (and (> (length k) 0)
                                            (not (mouse-event-p (aref k 0)))
                                            (not (memq (aref k 0)
                                                       '(menu-bar header-line
                                                         mode-line tab-line)))))
                                     keys)))
                      (key-description key))))))
    (if (listp cmd-or-cmds)
        (let ((keys (mapcar lookup cmd-or-cmds)))
          (if (seq-every-p #'identity keys)
              (string-join keys "/")
            (or (seq-find #'identity keys) "-")))
      (or (funcall lookup cmd-or-cmds) "-"))))

(defun egent-sidebar--insert-footer ()
  "Insert the key hints, padded so they sit flush against the window bottom."
  (let* ((win (get-buffer-window (current-buffer)))
         (used (line-number-at-pos (point)))
         (hints (length egent-sidebar--hints))
         (avail (and win (window-body-height win))))
    (when avail
      (insert (make-string (max 0 (- avail used hints)) ?\n)))
    (dolist (hint egent-sidebar--hints)
      (insert " "
              (propertize (egent-sidebar--format-hint-key (car hint))
                          'face 'egent-sidebar-hint-key)
              " "
              (propertize (cdr hint) 'face 'egent-sidebar-hint-desc)
              "\n"))))

(defun egent-sidebar--row-width (&optional reserved)
  "Return the columns a row's label may use, minus RESERVED.
Rows are indented four columns and carry an icon and a space."
  (max 1 (- egent-sidebar-width 6 (or reserved 0))))

(defun egent-sidebar--insert-buffer-row (buf root)
  "Insert a row for the live session BUF under ROOT."
  (push (list :type 'buffer :buffer buf :root root) egent-sidebar--entries)
  (insert (propertize (concat "    " (egent-icon (egent-buffer-state buf))
                              " " (egent-buffer-row-label
                                   buf (egent-sidebar--row-width))
                              "\n")
                      'egent-buffer buf)))

(defun egent-sidebar--insert-session-row (session config root)
  "Insert a row for the resumable SESSION of CONFIG under ROOT."
  (push (list :type 'session :session session :config config :root root)
        egent-sidebar--entries)
  (let ((time (egent-relative-time (map-elt session 'updatedAt))))
    (insert (propertize
             (concat "    " (egent-icon 'past) " "
                     (propertize
                      (egent-truncate (egent-session-label session)
                                      (egent-sidebar--row-width
                                       (if (string-empty-p time)
                                           0
                                         (+ 2 (string-width time)))))
                      'face 'egent-session)
                     (if (string-empty-p time)
                         ""
                       (concat "  " (propertize time 'face 'egent-session-time)))
                     "\n")
             'egent-session session))))

(defun egent-sidebar--insert-sessions (root)
  "Insert resumable session rows for every fetched agent in ROOT."
  (dolist (config (egent-session-agents-for-root root))
    (let ((identifier (map-elt config :identifier)))
      (cond
       ((egent-session-fetching-p root identifier)
        (insert (propertize (format "    … %s sessions\n" (egent-config-name config))
                            'face 'shadow)))
       ((egent-session-cached-p root identifier)
        (dolist (session (egent-session-resumable root identifier))
          (egent-sidebar--insert-session-row session config root)))))))

(defun egent-sidebar--render ()
  "Render the sidebar buffer and rebuild the entry list."
  (let ((groups (egent-grouped-buffers)))
    (with-current-buffer (get-buffer-create egent-sidebar--buffer-name)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (setq egent-sidebar--entries nil)
        (insert (propertize " Egent\n"
                            'face '(:inherit font-lock-function-name-face :weight bold)))
        (insert "\n")
        (dolist (group groups)
          (let* ((root (car group))
                 (pname (cadr group))
                 (bufs (caddr group))
                 (collapsed (member root egent-sidebar--collapsed)))
            (push (list :type 'project :root root) egent-sidebar--entries)
            (insert (propertize (concat " " (if collapsed "▸ " "▾ ") pname "\n")
                                'face 'egent-project
                                'egent-root root))
            (unless collapsed
              (dolist (buf bufs)
                (egent-sidebar--insert-buffer-row buf root))
              (egent-sidebar--insert-sessions root))
            (insert "\n")))
        (unless groups
          (insert (propertize "    No sessions yet — press s\n" 'face 'shadow)))
        (egent-sidebar--insert-footer)
        (setq egent-sidebar--entries (nreverse egent-sidebar--entries))
        (setq buffer-read-only t)
        (setq-local cursor-type nil))
      (use-local-map egent-sidebar-map))
    (setq egent-sidebar--state-snapshot (egent-sidebar--capture-states))))

;;;; Highlight

(defvar egent-sidebar--highlight-overlay nil
  "Overlay marking the selected entry.")

(defun egent-sidebar--entry-position (entry)
  "Return the buffer position of ENTRY's row, or nil."
  (pcase (plist-get entry :type)
    ('project (text-property-any (point-min) (point-max)
                                 'egent-root (plist-get entry :root)))
    ('buffer  (text-property-any (point-min) (point-max)
                                 'egent-buffer (plist-get entry :buffer)))
    ('session (text-property-any (point-min) (point-max)
                                 'egent-session (plist-get entry :session)))))

(defun egent-sidebar--highlight (idx)
  "Highlight entry IDX and sync point to its line."
  (with-current-buffer (get-buffer-create egent-sidebar--buffer-name)
    (let ((inhibit-read-only t))
      (unless (overlayp egent-sidebar--highlight-overlay)
        (setq egent-sidebar--highlight-overlay (make-overlay (point-min) (point-min))))
      (let ((ov egent-sidebar--highlight-overlay))
        (move-overlay ov (point-min) (point-min))
        (when-let* ((entry (nth idx egent-sidebar--entries))
                    (pos (egent-sidebar--entry-position entry)))
          (move-overlay ov pos
                        (min (1+ (save-excursion (goto-char pos) (line-end-position)))
                             (point-max)))
          (overlay-put ov 'face 'egent-sidebar-selection)
          (goto-char pos)
          (when-let* ((win (get-buffer-window (current-buffer))))
            (set-window-point win pos)))))))

;;;; Preview

(defun egent-sidebar--preview ()
  "Show the highlighted live session in the main window."
  (when-let* ((entry (egent-sidebar--entry))
              ((eq (plist-get entry :type) 'buffer))
              (shell-buf (plist-get entry :buffer))
              (disp-buf (egent-preferred-buffer shell-buf)))
    (when (and (window-live-p egent-sidebar--main-window)
               (buffer-live-p disp-buf))
      (set-window-buffer egent-sidebar--main-window disp-buf))))

;;;; Navigation

(defun egent-sidebar--navigable-p (entry)
  "Return non-nil when ENTRY can be reached with n/p.
Collapsing a project hides its rows, leaving the header as the only way
to expand it again, so headers are navigable exactly while collapsed."
  (or (memq (plist-get entry :type) '(buffer session))
      (member (plist-get entry :root) egent-sidebar--collapsed)))

(defun egent-sidebar--step (step)
  "Move the selection by STEP entries, skipping expanded project headers."
  (when egent-sidebar--entries
    (let ((len (length egent-sidebar--entries))
          (idx egent-sidebar--current-idx))
      (catch 'found
        (dotimes (_ len)
          (setq idx (mod (+ idx step) len))
          (when (egent-sidebar--navigable-p (nth idx egent-sidebar--entries))
            (throw 'found idx))))
      (setq egent-sidebar--current-idx idx)
      (egent-sidebar--highlight idx)
      (egent-sidebar--preview))))

(defun egent-sidebar-next ()
  "Move to the next entry and preview it."
  (interactive)
  (egent-sidebar--step 1))

(defun egent-sidebar-prev ()
  "Move to the previous entry and preview it."
  (interactive)
  (egent-sidebar--step -1))

;;;; Mouse

(defun egent-sidebar-mouse-select (event)
  "Move the selection to the row clicked by EVENT and preview it."
  (interactive "e")
  (with-current-buffer (get-buffer egent-sidebar--buffer-name)
    (let* ((pos (posn-point (event-start event)))
           (buf (and pos (get-text-property pos 'egent-buffer)))
           (session (and pos (get-text-property pos 'egent-session)))
           (root (and pos (get-text-property pos 'egent-root)))
           (idx (cond
                 (buf (cl-position-if (lambda (e)
                                        (eq (plist-get e :buffer) buf))
                                      egent-sidebar--entries))
                 (session (cl-position-if (lambda (e)
                                            (eq (plist-get e :session) session))
                                          egent-sidebar--entries))
                 (root (cl-position-if (lambda (e)
                                         (and (eq (plist-get e :type) 'project)
                                              (equal (plist-get e :root) root)))
                                       egent-sidebar--entries)))))
      (when idx
        (setq egent-sidebar--current-idx idx)
        (egent-sidebar--highlight idx)
        (egent-sidebar--preview)))))

(defun egent-sidebar-mouse-select-double (event)
  "Select and act on the row double-clicked by EVENT."
  (interactive "e")
  (egent-sidebar-mouse-select event)
  (egent-sidebar-select))

;;;; Commands

(defun egent-sidebar-select ()
  "Act on the highlighted entry.
Live session: focus it.  Past session: resume it.  Project: collapse."
  (interactive)
  (when-let* ((entry (egent-sidebar--entry)))
    (pcase (plist-get entry :type)
      ('project (egent-sidebar-collapse))
      ('buffer
       (egent-sidebar--preview)
       (when (window-live-p egent-sidebar--main-window)
         (select-window egent-sidebar--main-window)))
      ('session
       (let ((session (plist-get entry :session))
             (config (plist-get entry :config))
             (root (plist-get entry :root))
             (main egent-sidebar--main-window))
         (when (window-live-p main)
           (select-window main))
         (egent-session-resume :root root
                               :config config
                               :session-id (map-elt session 'sessionId)
                               :title (map-elt session 'title))
         (when-let* ((win (get-buffer-window egent-sidebar--buffer-name)))
           (select-window win)
           (egent-sidebar-refresh)))))))

(defun egent-sidebar-collapse ()
  "Toggle collapse of the highlighted entry's project group."
  (interactive)
  (when-let* ((entry (egent-sidebar--entry))
              (root (plist-get entry :root)))
    (if (member root egent-sidebar--collapsed)
        (setq egent-sidebar--collapsed (delete root egent-sidebar--collapsed))
      (push root egent-sidebar--collapsed))
    (egent-sidebar--render)
    (setq egent-sidebar--current-idx
          (or (cl-position-if (lambda (e)
                                (and (eq (plist-get e :type) 'project)
                                     (equal (plist-get e :root) root)))
                              egent-sidebar--entries)
              0))
    (egent-sidebar--highlight egent-sidebar--current-idx)))

(defun egent-sidebar-fetch-sessions ()
  "Ask the highlighted project's agents which sessions they remember."
  (interactive)
  (if-let* ((root (egent-sidebar--current-root)))
      (progn
        (egent-session-forget)
        (egent-session-refresh-project
         :root root
         :callback (lambda ()
                     (when (egent-sidebar--active-p)
                       (egent-sidebar-refresh))))
        (egent-sidebar-refresh))
    (user-error "No project selected")))

(defun egent-sidebar--auto-fetch ()
  "Fetch sessions for projects that have never been fetched.
Guarded on the cache so re-rendering cannot start a fetch loop."
  (when egent-sidebar-auto-fetch-sessions
    (dolist (group (egent-grouped-buffers))
      (let ((root (car group)))
        (dolist (config (egent-session-agents-for-root root))
          (let ((identifier (map-elt config :identifier)))
            (unless (or (egent-session-cached-p root identifier)
                        (egent-session-fetching-p root identifier))
              (egent-session-fetch
               :root root
               :config config
               :callback (lambda (_result)
                           (when (egent-sidebar--active-p)
                             (egent-sidebar-refresh)))))))))))

(defun egent-sidebar-name-session ()
  "Name the highlighted live session."
  (interactive)
  (when-let* ((entry (egent-sidebar--entry))
              ((eq (plist-get entry :type) 'buffer)))
    (egent-name-session (plist-get entry :buffer))))

(defun egent-sidebar-rename-session ()
  "Name the highlighted live session by hand."
  (interactive)
  (when-let* ((entry (egent-sidebar--entry))
              ((eq (plist-get entry :type) 'buffer)))
    (egent-rename-session (plist-get entry :buffer))))

(defun egent-sidebar-name-all-sessions ()
  "Name every live session in turn."
  (interactive)
  (dolist (buf (agent-shell-buffers))
    (egent-name-session buf)))

(define-obsolete-function-alias
  'egent-sidebar-label #'egent-sidebar-name-session "0.2.0")
(define-obsolete-function-alias
  'egent-sidebar-label-all #'egent-sidebar-name-all-sessions "0.2.0")

(defun egent-sidebar--kill-buffer-entry (entry)
  "Kill the live session ENTRY points at, along with its viewport buffer."
  (let* ((shell-buf (plist-get entry :buffer))
         (viewport (egent-preferred-buffer shell-buf)))
    (when (and (buffer-live-p viewport) (not (eq viewport shell-buf)))
      (kill-buffer viewport))
    (when (buffer-live-p shell-buf)
      (kill-buffer shell-buf))
    (egent-sidebar-refresh)))

(defun egent-sidebar--delete-session-entry (entry)
  "Make the agent forget the past session ENTRY points at.
Confirmed first: killing a shell only closes a window onto a session,
but this throws the session itself away."
  (let ((session (plist-get entry :session))
        (config (plist-get entry :config))
        (root (plist-get entry :root)))
    (when (yes-or-no-p (format "Delete session %s from %s? "
                               (egent-session-label session)
                               (egent-config-name config)))
      (egent-session-delete
       :root root
       :config config
       :session-id (map-elt session 'sessionId)
       :callback (lambda (error)
                   (egent-session-report-delete session error)
                   (when (egent-sidebar--active-p)
                     (egent-sidebar-refresh))))
      ;; The row is suppressed while the delete is in flight, so redraw now
      ;; rather than leaving it there to be deleted a second time.
      (egent-sidebar-refresh))))

(defun egent-sidebar-kill ()
  "Kill the highlighted live session, or delete the highlighted past one.
A past session has no buffer to kill: it is deleted from the agent's
history instead, which cannot be undone."
  (interactive)
  (when-let* ((entry (egent-sidebar--entry)))
    (pcase (plist-get entry :type)
      ('buffer (egent-sidebar--kill-buffer-entry entry))
      ('session (egent-sidebar--delete-session-entry entry)))))

(defun egent-sidebar-refresh ()
  "Re-render the sidebar."
  (interactive)
  (when (get-buffer egent-sidebar--buffer-name)
    (let ((buf (egent-sidebar--current-buffer)))
      (egent-sidebar--render)
      (egent-sidebar--populate-perspective)
      (egent-sidebar--restore-idx buf)
      (egent-sidebar--highlight egent-sidebar--current-idx)
      (egent-sidebar--preview))))

(defun egent-sidebar-reload ()
  "Re-ask every listed project's agents for sessions, then re-render.
`egent-sidebar-refresh' only redraws what is already cached, so a session
started, renamed or deleted outside Emacs stays invisible until the cache
is dropped — which is what pressing g is expected to do."
  (interactive)
  (egent-session-forget)
  (dolist (group (egent-grouped-buffers))
    (egent-session-refresh-project
     :root (car group)
     :callback (lambda ()
                 (when (egent-sidebar--active-p)
                   (egent-sidebar-refresh)))))
  ;; Redraw now so the rows a fetch is pending for show as such rather than
  ;; disappearing with the cache they were rendered from.
  (egent-sidebar-refresh))

(defun egent-sidebar-new-shell ()
  "Start a new shell in the highlighted project and show it."
  (interactive)
  (when (window-live-p egent-sidebar--main-window)
    (let ((before (agent-shell-buffers))
          (wconf (current-window-configuration))
          (root (egent-sidebar--current-root)))
      (with-selected-window egent-sidebar--main-window
        ;; `default-directory' is buffer-local, so binding it outside the
        ;; window switch would be undone by the main window's own buffer.
        (let ((default-directory (or root default-directory)))
          (agent-shell-new-shell)))
      (set-window-configuration wconf)
      (when-let* ((new-buf (seq-find (lambda (b) (not (memq b before)))
                                     (agent-shell-buffers)))
                  ((buffer-live-p new-buf)))
        (set-window-buffer egent-sidebar--main-window new-buf)))
    (when-let* ((win (get-buffer-window egent-sidebar--buffer-name)))
      (select-window win)
      (egent-sidebar-refresh))))

;;;; Setup / teardown

(defun egent-sidebar--setup ()
  "Build the sidebar and main window layout."
  (when (window-parameter (selected-window) 'window-side)
    (when-let* ((non-side (seq-find (lambda (w) (not (window-parameter w 'window-side)))
                                   (window-list))))
      (select-window non-side)))
  (delete-other-windows)
  (let ((sidebar-win
         (display-buffer-in-side-window
          (get-buffer-create egent-sidebar--buffer-name)
          `((side . left)
            (window-width . ,egent-sidebar-width)
            (window-parameters . ((no-delete-other-windows . t)))))))
    (setq egent-sidebar--main-window
          (car (seq-filter (lambda (w) (not (eq w sidebar-win))) (window-list)))))
  (setq egent-sidebar--current-idx 0
        egent-sidebar--collapsed nil)
  (egent-sidebar--render)
  (egent-sidebar--populate-perspective)
  (egent-sidebar--highlight 0)
  (egent-sidebar--preview)
  (select-window (get-buffer-window egent-sidebar--buffer-name))
  (egent-sidebar--auto-fetch)
  (setq egent-sidebar--refresh-timer
        (run-with-timer 2 2 #'egent-sidebar--maybe-refresh)))

(defun egent-sidebar--teardown ()
  "Tear down the sidebar window and reset state."
  (when (timerp egent-sidebar--refresh-timer)
    (cancel-timer egent-sidebar--refresh-timer))
  (setq egent-sidebar--refresh-timer nil)
  (when-let* ((win (get-buffer-window egent-sidebar--buffer-name)))
    (delete-window win))
  (setq egent-sidebar--entries nil
        egent-sidebar--current-idx 0
        egent-sidebar--collapsed nil
        egent-sidebar--main-window nil
        egent-sidebar--state-snapshot nil))

;;;; Entry points

;;;###autoload
(defun egent-sidebar-focus ()
  "Focus the sidebar, opening the workspace when it is not shown."
  (interactive)
  (if-let* ((win (get-buffer-window egent-sidebar--buffer-name)))
      (select-window win)
    (egent-sidebar-toggle)))

;;;###autoload
(defun egent-sidebar-toggle ()
  "Toggle the egent workspace.

Opens a sidebar listing every agent-shell session grouped by project:
those open in a buffer, and those the agent still remembers, which are
resumed by selecting them.  Calling it again restores the previous
layout.

Sidebar keys:
  n/p    navigate and preview
  RET    focus session / resume past session / toggle project
  TAB    collapse or expand a project group
  S      re-ask the project's agents for past sessions
  o      resume a session in another project
  r      name the current session
  R      name every session
  M-r    rename the current session by hand
  K      kill the current session / delete a past one
  g      refetch every project's sessions and redraw
  s      new shell in the current project
  q      quit"
  (interactive)
  (if (egent-sidebar--active-p)
      (progn (egent-sidebar--teardown)
             (egent-sidebar--exit-workspace))
    (egent-sidebar--enter-workspace)
    (egent-sidebar--setup)))

(provide 'egent-sidebar)
;;; egent-sidebar.el ends here
