;;; egent-term.el --- Agent sessions in a native terminal  -*- lexical-binding: t -*-

;; Copyright (C) 2026

;; Author: Sreenivas Venkobarao
;; Package-Requires: ((emacs "29.1") (agent-shell "0.66.1") (ghostel "0.34"))

;;; Commentary:

;; Runs an agent's own TUI (pi, by default) in a ghostel terminal instead
;; of an `agent-shell' buffer, either as a new session or resuming one
;; egent lists.
;;
;; A new session is started under an id egent picks, so the buffer knows
;; its session from the first moment instead of waiting for the agent to
;; report one.  That id is what hides the session from the resumable list
;; and makes a second resume switch to the open terminal.

;;; Code:

(require 'cl-lib)
(require 'egent-core)
(require 'egent-session)
(require 'map)
(require 'project)

(declare-function ghostel-exec "ghostel" (buffer program &optional args))

;;;; Customization

(defcustom egent-term-commands
  '((pi :new ("pi" "--session-id" "%s")
        :resume ("pi" "--session" "%s")))
  "How to run each agent in a terminal, keyed by agent identifier.
`:new' starts a session under a given id and `:resume' reopens one; in
both, \"%s\" is replaced by the session id.  pi's `--session-id' would
silently create a session it cannot find, so resuming uses `--session'."
  :type '(alist :key-type symbol
                :value-type (plist :key-type symbol
                                   :value-type (repeat string)))
  :group 'egent)

(defcustom egent-term-agent 'pi
  "Agent identifier `egent-term-new' starts.
Must have an entry in `egent-term-commands'."
  :type 'symbol
  :group 'egent)

(defcustom egent-term-buffer-title-width 40
  "Columns of a resumed session's title kept in its buffer name."
  :type 'integer
  :group 'egent)

;;;; Starting

(defun egent-term--argv (identifier key session-id)
  "Return the argv running IDENTIFIER's KEY command for SESSION-ID, or nil."
  (when-let* ((template (plist-get (alist-get identifier egent-term-commands)
                                   key)))
    (mapcar (lambda (arg) (string-replace "%s" session-id arg)) template)))

(defun egent-term--new-session-id ()
  "Return a fresh random UUID to start a session under."
  (let ((bytes (mapcar (lambda (_) (random 256)) (make-list 16 nil))))
    ;; Version 4, RFC 4122 variant.
    (setf (nth 6 bytes) (logior #x40 (logand (nth 6 bytes) #x0f))
          (nth 8 bytes) (logior #x80 (logand (nth 8 bytes) #x3f)))
    (apply #'format
           "%02x%02x%02x%02x-%02x%02x-%02x%02x-%02x%02x-%02x%02x%02x%02x%02x%02x"
           bytes)))

(defun egent-term--buffer-name (identifier root title)
  "Return a buffer name for IDENTIFIER in ROOT, with TITLE when known."
  (let ((project (file-name-nondirectory (directory-file-name root)))
        (title (egent-nonempty (egent-one-line title))))
    (format "*%s: %s%s*" identifier project
            (if title
                (concat " · " (egent-truncate title egent-term-buffer-title-width))
              ""))))

(defun egent-term--start (root identifier key session-id &optional title)
  "Run IDENTIFIER's KEY command for SESSION-ID in a terminal in ROOT.
TITLE, when known, goes into the buffer name.  Returns the buffer."
  (require 'ghostel)
  (let* ((argv (or (egent-term--argv identifier key session-id)
                   (user-error "egent: no terminal command for %s" identifier)))
         (buffer (generate-new-buffer
                  (egent-term--buffer-name identifier root title))))
    (with-current-buffer buffer
      (setq default-directory (file-name-as-directory (expand-file-name root))))
    ;; Shown before spawning, since ghostel sizes the terminal to its window.
    (pop-to-buffer-same-window buffer)
    (condition-case err
        (ghostel-exec buffer (car argv) (cdr argv))
      ((error quit)
       (kill-buffer buffer)
       (signal (car err) (cdr err))))
    (with-current-buffer buffer
      (setq egent-term-session-id session-id))
    buffer))

;;;###autoload
(cl-defun egent-term-resume-session (&key root config session-id title)
  "Resume SESSION-ID, one of CONFIG's sessions for ROOT, in a terminal.
TITLE, when known, goes into the buffer name.  Switches to the buffer
already attached to the session, shell or terminal, instead of opening a
second client on it."
  (if-let* ((existing (egent-session-buffer session-id)))
      (pop-to-buffer (egent-preferred-buffer existing))
    (egent-term--start root (map-elt config :identifier) :resume session-id
                       title)))

;;;; Commands

(defun egent-term--read-config (root)
  "Return the agent config to resume a session of ROOT with in a terminal.
Only agents with a `:resume' command qualify; prompts when several do."
  (let* ((usable (lambda (config)
                   (plist-get (alist-get (map-elt config :identifier)
                                         egent-term-commands)
                              :resume)))
         (configs (or (seq-filter usable (egent-session-agents-for-root root))
                      (seq-filter usable (egent-agent-configs)))))
    (or (if (length= configs 1)
            (car configs)
          (let* ((choices (mapcar (lambda (c) (cons (egent-config-name c) c))
                                  configs))
                 (pick (completing-read "Agent: " (mapcar #'car choices) nil t)))
            (alist-get pick choices nil nil #'equal)))
        (user-error "egent: no agent in `egent-term-commands' can resume"))))

;;;###autoload
(defun egent-term-new (&optional root)
  "Start a new `egent-term-agent' session in a terminal in ROOT.
ROOT defaults to the current project, prompting when there is none."
  (interactive)
  (let ((root (or root (project-root (project-current t)))))
    (egent-term--start root egent-term-agent :new
                       (egent-term--new-session-id))))

;;;###autoload
(defun egent-term-resume (&optional root)
  "Pick a past session for ROOT and resume it in a terminal."
  (interactive)
  (let* ((root (egent-session--read-root "Resume session in project: " root))
         (config (egent-term--read-config root)))
    (egent-session--pick
     :root root
     :config config
     :prompt "Resume session in terminal: "
     :callback (lambda (session)
                 (egent-term-resume-session
                  :root root
                  :config config
                  :session-id (map-elt session 'sessionId)
                  :title (map-elt session 'title))))))

(provide 'egent-term)
;;; egent-term.el ends here
