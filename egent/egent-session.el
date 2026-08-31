;;; egent-session.el --- Resumable session discovery for egent  -*- lexical-binding: t -*-

;; Copyright (C) 2026

;; Author: Sreenivas Venkobarao
;; Package-Requires: ((emacs "29.1") (agent-shell "0.66.1") (acp "0.13.1"))

;;; Commentary:

;; Lists the sessions an agent remembers for a project, including the ones
;; that have no Emacs buffer, so they can be resumed.
;;
;; `agent-shell' cannot answer this question for us.  It only issues
;; `session/list' while a shell is bootstrapping, and the events it emits
;; around that (`session-list', `session-selected') carry no payload, so
;; there is nothing to subscribe to.  Instead we drive a short-lived ACP
;; client directly: initialize, ask, shut down.  That keeps this agnostic
;; about where each agent stores its history, which reading the on-disk
;; formats directly (pi's JSONL, Claude's projects directory) would not.
;;
;; Every fetch is asynchronous.  `acp-send-request' does support `:sync',
;; but its wait loop has no timeout, so an agent that never answers would
;; hang Emacs with no way out.

;;; Code:

(require 'acp)
(require 'agent-shell)
(require 'cl-lib)
(require 'egent-core)
(require 'map)
(require 'seq)

;;;; Customization

(defcustom egent-session-agents 'auto
  "Which agents to query for resumable sessions.

`auto' queries only the agents that already have a live buffer in the
project, which keeps a sidebar refresh to one subprocess per agent you
actually use there.  Alternatively, a list of agent identifier symbols
\(e.g. \\='(pi claude-code)) queries those regardless."
  :type '(choice (const :tag "Agents already used in the project" auto)
                 (repeat :tag "Specific agents" symbol))
  :group 'egent)

(defcustom egent-session-timeout 20
  "Seconds to wait for an agent to answer before giving up on it.
An agent that needs authentication, or one whose adapter is missing, can
otherwise leave a fetch pending forever."
  :type 'number
  :group 'egent)

;;;; Cache

(defvar egent-session--cache (make-hash-table :test 'equal)
  "Maps (ROOT . IDENTIFIER) to a plist of :sessions and :error.
Fetches are explicit, so a stale entry is preferable to spawning a
subprocess during redisplay.")

(defvar egent-session--inflight (make-hash-table :test 'equal)
  "Set of (ROOT . IDENTIFIER) keys with a fetch in progress.")

(defun egent-session--key (root identifier)
  "Return the cache key for ROOT and IDENTIFIER."
  (cons (directory-file-name (expand-file-name root)) identifier))

(defun egent-session-cached (root identifier)
  "Return the cached session list for ROOT and IDENTIFIER, or nil."
  (plist-get (gethash (egent-session--key root identifier) egent-session--cache)
             :sessions))

(defun egent-session-cached-p (root identifier)
  "Return non-nil when ROOT and IDENTIFIER have been fetched at least once."
  (and (gethash (egent-session--key root identifier) egent-session--cache) t))

(defun egent-session-fetching-p (root identifier)
  "Return non-nil when a fetch for ROOT and IDENTIFIER is in flight."
  (and (gethash (egent-session--key root identifier) egent-session--inflight) t))

(defun egent-session-cached-title (session-id)
  "Return the title a fetch cached for SESSION-ID, or nil.
A session restored outside egent — through `agent-shell''s own picker,
say — is never given a title, and the agent only refreshes one when it
matches the shell's working directory to its own record of it.  The list
egent fetched for the sidebar has the title eitherway."
  (when session-id
    (catch 'found
      (maphash (lambda (_key entry)
                 (dolist (session (plist-get entry :sessions))
                   (when (equal (map-elt session 'sessionId) session-id)
                     (throw 'found (egent-nonempty
                                    (egent-one-line
                                     (map-elt session 'title)))))))
               egent-session--cache)
      nil)))

(defun egent-session-retitle-cached (session-id title)
  "Record TITLE as the cached title of SESSION-ID.
The agent has been told the new name, but the session list was fetched
before that: without this, renaming a session and then closing its
buffer would bring it back under its old title until the next fetch."
  (when (and session-id (egent-nonempty title))
    (maphash (lambda (_key entry)
               (dolist (session (plist-get entry :sessions))
                 (when (and (consp session)
                            (equal (map-elt session 'sessionId) session-id))
                   (if-let* ((cell (assq 'title session)))
                       (setcdr cell title)
                     (nconc session (list (cons 'title title)))))))
             egent-session--cache)))

(defun egent-session-forget (&optional root identifier)
  "Drop cached sessions.
With ROOT and IDENTIFIER, drop just that entry; otherwise drop everything."
  (if (and root identifier)
      (remhash (egent-session--key root identifier) egent-session--cache)
    (clrhash egent-session--cache)))

;;;; Fetch

(defun egent-session--sort (sessions)
  "Return SESSIONS sorted newest first.
ISO-8601 timestamps sort lexically the same way they sort chronologically."
  (seq-sort (lambda (a b)
              (string> (or (map-elt a 'updatedAt) (map-elt a 'createdAt) "")
                       (or (map-elt b 'updatedAt) (map-elt b 'createdAt) "")))
            (copy-sequence sessions)))

(defun egent-session--error-string (error)
  "Return a readable message for ERROR.
Fetches fail as Lisp errors (missing adapter), as ACP error objects
\(authentication, unsupported method) or as our own strings."
  (cond
   ((stringp error) error)
   ((and (consp error) (symbolp (car error)) (get (car error) 'error-conditions))
    (error-message-string error))
   ((and (listp error) (map-elt error 'message))
    (format "%s" (map-elt error 'message)))
   (t (format "%s" error))))

(defun egent-session--supports-list-p (response)
  "Return non-nil when an initialize RESPONSE advertises `session/list'."
  (let ((caps (or (map-elt response 'sessionCapabilities)
                  (map-nested-elt response '(agentCapabilities sessionCapabilities)))))
    (and (listp caps) (assq 'list caps) t)))

(cl-defun egent-session-fetch (&key root config callback)
  "Ask CONFIG's agent which sessions it remembers for ROOT.

CALLBACK is called with (SESSIONS . ERROR); SESSIONS is a list of ACP
session alists (newest first) and ERROR is nil or a string.  It is
called exactly once, including on timeout."
  (let* ((root (directory-file-name (expand-file-name root)))
         (identifier (map-elt config :identifier))
         (key (egent-session--key root identifier))
         (context (generate-new-buffer (format " *egent-session-fetch %s*" identifier)))
         (client nil)
         (timer nil)
         (settled nil))
    (cl-labels
        ((finish (sessions error)
           (unless settled
             (setq settled t)
             (remhash key egent-session--inflight)
             (when (timerp timer) (cancel-timer timer))
             ;; Shut the client down before killing its context buffer:
             ;; `acp' resolves callbacks with `with-current-buffer', which
             ;; would error on a dead buffer if a late reply arrived.
             (when client (ignore-errors (acp-shutdown :client client)))
             (when (buffer-live-p context) (kill-buffer context))
             (puthash key (list :sessions sessions :error error) egent-session--cache)
             (when callback (funcall callback (cons sessions error)))))
         (fail (error)
           (finish nil (egent-session--error-string error))))
      (puthash key t egent-session--inflight)
      (setq timer (run-at-time egent-session-timeout nil
                               (lambda () (fail "timed out"))))
      (condition-case err
          (let ((default-directory (file-name-as-directory root)))
            (setq client (funcall (map-elt config :client-maker) context))
            (acp-send-request
             :client client
             :buffer context
             :request (acp-make-initialize-request
                       :protocol-version 1
                       :client-info '((name . "egent")
                                      (title . "Egent")
                                      (version . "0.1.0"))
                       :read-text-file-capability nil
                       :write-text-file-capability nil)
             :on-failure (lambda (error) (fail error))
             :on-success
             (lambda (response)
               (if (not (egent-session--supports-list-p response))
                   (finish nil "agent does not support session/list")
                 (condition-case err
                     (acp-send-request
                      :client client
                      :buffer context
                      :request (acp-make-session-list-request :cwd root)
                      :on-failure (lambda (error) (fail error))
                      :on-success
                      (lambda (response)
                        (finish (egent-session--sort
                                 (append (or (map-elt response 'sessions) '()) nil))
                                nil)))
                   (error (fail err)))))))
        (error (fail err))))))

(defun egent-session-agents-for-root (root)
  "Return the agent configs to query for ROOT, honouring `egent-session-agents'."
  (if (listp egent-session-agents)
      (delq nil (mapcar #'egent-config-by-identifier egent-session-agents))
    (let ((identifiers
           (seq-uniq
            (delq nil
                  (mapcar (lambda (buf)
                            (when (equal (with-current-buffer buf (agent-shell-cwd))
                                         root)
                              (egent-buffer-agent-identifier buf)))
                          (agent-shell-buffers))))))
      (delq nil (mapcar #'egent-config-by-identifier identifiers)))))

(cl-defun egent-session-refresh-project (&key root callback)
  "Fetch sessions for every applicable agent in ROOT.
CALLBACK is called with no arguments after each agent answers, so a
caller can re-render incrementally."
  (let ((configs (egent-session-agents-for-root root)))
    (if (null configs)
        (when callback (funcall callback))
      (dolist (config configs)
        (egent-session-fetch
         :root root
         :config config
         :callback (lambda (result)
                     (when (cdr result)
                       (message "egent: %s sessions unavailable (%s)"
                                (egent-config-name config) (cdr result)))
                     (when callback (funcall callback))))))))

;;;; Open sessions

(defun egent-session-buffer (session-id)
  "Return the live shell buffer attached to SESSION-ID, or nil."
  (seq-find (lambda (buf)
              (equal (egent-buffer-session-id buf) session-id))
            (agent-shell-buffers)))

(defvar egent-session--resuming (make-hash-table :test 'equal)
  "Maps a session id to the time a resume was started for it.
A resumed shell only learns its session id once bootstrapping finishes,
so without this a second RET in that window would start a second shell
against the same session.")

(defconst egent-session--resuming-grace 60
  "Seconds a pending resume suppresses its row before it is shown again.
Bounded so a resume that never completes (failed authentication, missing
adapter) cannot hide the session for the rest of the Emacs session.")

(defun egent-session--pending-p (session-id)
  "Return non-nil when SESSION-ID has a resume in progress."
  (when-let* ((started (gethash session-id egent-session--resuming)))
    (if (< (- (float-time) started) egent-session--resuming-grace)
        t
      (remhash session-id egent-session--resuming)
      nil)))

(defun egent-session-resumable (root identifier)
  "Return cached sessions for ROOT and IDENTIFIER that can still be resumed.
Excludes sessions already open in a buffer and those mid-resume."
  (seq-remove (lambda (session)
                (let ((id (map-elt session 'sessionId)))
                  (cond
                   ((egent-session-buffer id)
                    ;; The shell reported its id, so the resume is done.
                    (remhash id egent-session--resuming)
                    t)
                   ((egent-session--pending-p id) t))))
              (egent-session-cached root identifier)))

;;;; Resume

(defun egent-session--seed-title (buf title)
  "Record TITLE as BUF's session title.
`agent-shell' carries the title through `session/load' but never derives
one from it, so a resumed session would otherwise show its buffer name
until the agent is asked again on the first completed turn."
  (when-let* (((buffer-live-p buf))
              (title (egent-nonempty (egent-one-line title))))
    (with-current-buffer buf
      (agent-shell--set-session-title title))))

(cl-defun egent-session-resume (&key root config session-id title)
  "Start a shell in ROOT that resumes SESSION-ID using CONFIG.
TITLE, when known, labels the shell until the agent reports its own.
Switches to the existing buffer when the session is already open, since
resuming twice would leave two shells fighting over one session."
  (if-let* ((existing (egent-session-buffer session-id)))
      (pop-to-buffer (egent-preferred-buffer existing))
    (puthash session-id (float-time) egent-session--resuming)
    ;; `agent-shell--start' derives its cwd from `default-directory', which
    ;; is buffer-local, so this has to be bound around the call itself.
    (let ((default-directory (file-name-as-directory
                              (expand-file-name (or root default-directory)))))
      (egent-session--seed-title
       (agent-shell-start :config config :session-id session-id)
       title))))

(defun egent-session-label (session)
  "Return a display label for SESSION."
  (let ((title (egent-one-line (map-elt session 'title)))
        (id (or (map-elt session 'sessionId) "")))
    (or (egent-nonempty title)
        (format "(untitled %s)" (substring id 0 (min 8 (length id)))))))

;;;###autoload
(defun egent-resume (&optional root)
  "Pick a past session for ROOT and resume it.

Prompts for the project directory and agent when they cannot be inferred,
which is the only way to reach a project that has no session open."
  (interactive)
  (let* ((root (directory-file-name
                (expand-file-name
                 (or root
                     (read-directory-name "Resume session in project: "
                                          (ignore-errors (agent-shell-cwd)))))))
         (configs (or (egent-session-agents-for-root root)
                      (egent-agent-configs)))
         (config (if (length= configs 1)
                     (car configs)
                   (let* ((choices (mapcar (lambda (c)
                                             (cons (egent-config-name c) c))
                                           configs))
                          (pick (completing-read "Agent: " (mapcar #'car choices) nil t)))
                     (alist-get pick choices nil nil #'equal)))))
    (unless config
      (user-error "No agent selected"))
    (message "egent: asking %s for sessions in %s…"
             (egent-config-name config) (abbreviate-file-name root))
    (egent-session-fetch
     :root root
     :config config
     :callback
     (lambda (result)
       (let ((sessions (car result))
             (error (cdr result)))
         (cond
          (error (user-error "egent: %s" error))
          ((null sessions) (user-error "egent: no sessions for %s"
                                       (abbreviate-file-name root)))
          (t
           (let* ((width (apply #'max (mapcar (lambda (s)
                                                (length (egent-session-label s)))
                                              sessions)))
                  (choices
                   (mapcar (lambda (session)
                             (cons (concat
                                    (string-pad (egent-session-label session) (1+ width))
                                    (propertize (egent-relative-time
                                                 (map-elt session 'updatedAt))
                                                'face 'egent-session-time))
                                   session))
                           sessions))
                  ;; Completion frameworks append "(nil)" to candidates
                  ;; unless `this-command' is bound during the read.
                  (this-command 'egent-resume)
                  (pick (completing-read
                         "Resume session: "
                         (lambda (string pred action)
                           (if (eq action 'metadata)
                               '(metadata (display-sort-function . identity))
                             (complete-with-action action (mapcar #'car choices)
                                                   string pred)))
                         nil t))
                  (session (alist-get pick choices nil nil #'equal)))
             (egent-session-resume :root root
                                   :config config
                                   :session-id (map-elt session 'sessionId)
                                   :title (map-elt session 'title))))))))))

(provide 'egent-session)
;;; egent-session.el ends here
