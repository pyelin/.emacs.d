;;; egent-session-name.el --- Session naming for egent  -*- lexical-binding: t -*-

;; Copyright (C) 2026

;; Author: Sreenivas Venkobarao
;; Package-Requires: ((emacs "29.1") (agent-shell "0.66.1"))

;;; Commentary:

;; Names a session by handing its recent transcript to an external CLI and
;; renaming the buffer to whatever comes back.

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

;;;; Public API

;;;###autoload
(defun egent-name-session (&optional shell-buf)
  "Name SHELL-BUF by passing its recent content to an external command.
Interactively, resolves the shell buffer from the current agent-shell
context."
  (interactive)
  (let* ((buf (or shell-buf
                  (agent-shell--shell-buffer :no-create t :no-error t)
                  (user-error "No agent-shell buffer found")))
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
                         ;; `agent-shell'/`shell-maker' resolve a buffer's
                         ;; process by name, so a plain `rename-buffer'
                         ;; would detach the session from its process.
                         (let ((old-viewport
                                (get-buffer
                                 (concat (buffer-name buf) " [viewport]"))))
                           (shell-maker-set-buffer-name buf name)
                           (when (buffer-live-p old-viewport)
                             (with-current-buffer old-viewport
                               (rename-buffer
                                (concat name " [viewport]") t))))
                         (message "egent-name-session: named %s" name)
                         (when (fboundp 'egent-sidebar-refresh)
                           (egent-sidebar-refresh)))))))))
      ;; Close stdin so the subprocess doesn't wait for piped input.
      (process-send-eof proc))))

;;;; Compatibility

(define-obsolete-function-alias
  'egent-label #'egent-name-session "0.2.0")

(provide 'egent-session-name)
;;; egent-session-name.el ends here
