;;; egent-session-name.el --- Session naming for egent  -*- lexical-binding: t -*-

;; Copyright (C) 2026

;; Author: Sreenivas Venkobarao
;; Package-Requires: ((emacs "29.1") (agent-shell "0.66.1"))

;;; Commentary:

;; Names a session, either by hand or by handing its recent transcript to
;; an external CLI and taking whatever comes back.
;;
;; A session name is egent's own: the buffer keeps the name `agent-shell'
;; gave it, so `switch-to-buffer' still finds it, and the agent stays free
;; to rewrite its title on every turn.
;;
;; That name lives in the buffer, so it dies with it.  Every past session
;; comes from the list the agent answers `session/list' with, under the
;; agent's own title.  So the name is passed on to the agent too, through
;; whichever slash command it advertises for naming a session (pi: `/name'),
;; which is the only place a name outlives the buffer.

;;; Code:

(require 'agent-shell)
(require 'egent-core)
(require 'egent-session)
(require 'map)
(require 'seq)

;;;; Customization

(define-obsolete-variable-alias
  'egent-label-command 'egent-session-name-command "0.2.0")
(define-obsolete-variable-alias
  'egent-label-context-chars 'egent-session-name-context-chars "0.2.0")
(define-obsolete-variable-alias
  'egent-label-prompt 'egent-session-name-prompt "0.2.0")

(defcustom egent-session-name-command '("claude" "-p" "--model" "haiku")
  "Command and arguments for the session-naming subprocess.
The prompt text is appended as the final argument.

Examples:
  \\='(\"claude\" \"-p\" \"--model\" \"haiku\")
  \\='(\"llm\")
  \\='(\"ollama\" \"run\" \"llama3.2\")"
  :type '(repeat string)
  :group 'egent)

(defcustom egent-session-name-context-chars 2000
  "Characters of buffer content sent to the session namer.
Taken from the end of the buffer: every session opens with the same
welcome banner, which would otherwise be most of what the namer sees."
  :type 'integer
  :group 'egent)

(defcustom egent-session-name-agent-commands '("name" "rename" "title")
  "Slash commands that ask an agent to record a session name, best first.
The first one an agent advertises is submitted, so drop any an agent you
use means something else by.  An agent that advertises none keeps its own
title, and the name is remembered for as long as the buffer lives."
  :type '(repeat string)
  :group 'egent)

(defcustom egent-session-name-prompt
  "Reply with ONLY a terse 8-10 word title for this conversation, \
lowercase, no punctuation:\n\n%s"
  "Format string for the session-naming prompt.
%s is replaced with the buffer context."
  :type 'string
  :group 'egent)

;;;; Internals

(defun egent-session-name--agent-command (buf)
  "Return the naming command BUF's agent advertises, or nil."
  (let ((available (egent-buffer-available-commands buf)))
    (seq-find (lambda (command)
                (seq-some (lambda (entry)
                            (equal (map-elt entry 'name) command))
                          available))
              egent-session-name-agent-commands)))

(defun egent-session-name--tell-agent (buf name)
  "Ask BUF's agent to record NAME, and return non-nil when it was asked.
The command goes in as an ordinary prompt, which is how a slash command
reaches an agent over ACP; agents answer it themselves rather than
spending a turn on it.  A busy shell is left alone: interrupting a turn
to rename it would cost more than the name is worth."
  (when-let* ((name (egent-nonempty name))
              (command (egent-session-name--agent-command buf))
              ((buffer-live-p buf)))
    (if (with-current-buffer buf (shell-maker-busy))
        (progn
          (message "egent: %s is busy; the new name stays in Emacs"
                   (buffer-name buf))
          nil)
      (agent-shell--insert-to-shell-buffer
       :shell-buffer buf
       :text (concat "/" command " " name)
       :submit t
       :no-focus t)
      t)))

(defun egent-session-name--set (buf name)
  "Name the session BUF NAME and refresh whatever is displaying it.
The agent is told as well, when it has a command for it, so the name
survives the buffer: the sessions egent lists once a buffer is gone are
the agent's, titled by the agent."
  (egent-set-buffer-session-name buf name)
  (when (egent-session-name--tell-agent buf name)
    (egent-session-retitle-cached (egent-buffer-session-id buf) name))
  (force-mode-line-update t)
  (when (fboundp 'egent-sidebar-refresh)
    (egent-sidebar-refresh)))

(defun egent-session-name--buffer (shell-buf)
  "Return SHELL-BUF, or the shell buffer of the current context."
  (or shell-buf
      (agent-shell--shell-buffer :no-create t :no-error t)
      (user-error "No agent-shell buffer found")))

;;;; Public API

;;;###autoload
(defun egent-rename-session (&optional shell-buf name)
  "Name SHELL-BUF's session NAME, prompting when NAME is not given.
The prompt is seeded with the current name; answering with an empty one
clears it, putting the session back under the agent's own title."
  (interactive)
  (let* ((buf (egent-session-name--buffer shell-buf))
         (name (or name
                   (read-string
                    "Session name: "
                    (or (egent-buffer-session-name buf)
                        (egent-one-line (egent-buffer-session-title buf))
                        "")))))
    (egent-session-name--set buf (string-trim name))
    (message "egent: %s" (egent-buffer-label buf))))

;;;###autoload
(defun egent-name-session (&optional shell-buf)
  "Name SHELL-BUF by passing its recent content to an external command.
Interactively, resolves the shell buffer from the current agent-shell
context."
  (interactive)
  (let* ((buf (egent-session-name--buffer shell-buf))
         (context
          (with-current-buffer buf
            (buffer-substring-no-properties
             (max (point-min)
                  (- (point-max) egent-session-name-context-chars))
             (point-max))))
         (prompt (format egent-session-name-prompt context))
         (output ""))
    (message "egent-name-session: naming %s…" (buffer-name buf))
    (let ((proc (make-process
                 :name "egent-name-session"
                 :buffer nil
                 :command (append egent-session-name-command (list prompt))
                 :connection-type 'pipe
                 :filter (lambda (_proc chunk)
                           (setq output (concat output chunk)))
                 :sentinel
                 (lambda (_proc event)
                   (when (string-prefix-p "finished" event)
                     (let ((name (string-trim output)))
                       (when (and (buffer-live-p buf) (> (length name) 0))
                         (egent-session-name--set buf name)
                         (message "egent-name-session: named %s" name))))))))
      ;; Close stdin so the subprocess doesn't wait for piped input.
      (process-send-eof proc))))

;;;; Compatibility

(define-obsolete-function-alias
  'egent-label #'egent-name-session "0.2.0")

(provide 'egent-session-name)
;;; egent-session-name.el ends here
