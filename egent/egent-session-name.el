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

;;; Code:

(require 'agent-shell)
(require 'egent-core)

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

(defcustom egent-session-name-prompt
  "Reply with ONLY a terse 8-10 word title for this conversation, \
lowercase, no punctuation:\n\n%s"
  "Format string for the session-naming prompt.
%s is replaced with the buffer context."
  :type 'string
  :group 'egent)

;;;; Internals

(defun egent-session-name--set (buf name)
  "Name the session BUF NAME and refresh whatever is displaying it."
  (egent-set-buffer-session-name buf name)
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
