;;; egent-label.el --- Session labelling for egent  -*- lexical-binding: t -*-

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

(defcustom egent-label-command '("claude" "-p" "--model" "haiku")
  "Command and arguments for the titling subprocess.
The prompt text is appended as the final argument.

Examples:
  \\='(\"claude\" \"-p\" \"--model\" \"haiku\")
  \\='(\"llm\")
  \\='(\"ollama\" \"run\" \"llama3.2\")"
  :type '(repeat string)
  :group 'egent)

(defcustom egent-label-context-chars 2000
  "Characters of buffer content sent as context to the titler.
Taken from the end of the buffer: every session opens with the same
welcome banner, which would otherwise be most of what the titler sees."
  :type 'integer
  :group 'egent)

(defcustom egent-label-prompt
  "Reply with ONLY a terse 8-10 word title for this conversation, \
lowercase, no punctuation:\n\n%s"
  "Format string for the titling prompt.
%s is replaced with the buffer context."
  :type 'string
  :group 'egent)

;;;; Public API

;;;###autoload
(defun egent-label (&optional shell-buf)
  "Title SHELL-BUF by passing its recent content to `egent-label-command'.
Interactively, resolves the shell buffer from the current agent-shell
context."
  (interactive)
  (let* ((buf (or shell-buf
                  (agent-shell--shell-buffer :no-create t :no-error t)
                  (user-error "No agent-shell buffer found")))
         (context (with-current-buffer buf
                    (buffer-substring-no-properties
                     (max (point-min) (- (point-max) egent-label-context-chars))
                     (point-max))))
         (prompt (format egent-label-prompt context))
         (output ""))
    (message "egent-label: labeling %s…" (buffer-name buf))
    (let ((proc (make-process
                 :name "egent-label"
                 :buffer nil
                 :command (append egent-label-command (list prompt))
                 :connection-type 'pipe
                 :filter (lambda (_proc chunk)
                           (setq output (concat output chunk)))
                 :sentinel
                 (lambda (_proc event)
                   (when (string-prefix-p "finished" event)
                     (let ((title (string-trim output)))
                       (when (and (buffer-live-p buf) (> (length title) 0))
                         ;; `agent-shell'/`shell-maker' resolve a buffer's
                         ;; process by name, so a plain `rename-buffer'
                         ;; would detach the session from its process.
                         (let ((old-viewport
                                (get-buffer (concat (buffer-name buf) " [viewport]"))))
                           (shell-maker-set-buffer-name buf title)
                           (when (buffer-live-p old-viewport)
                             (with-current-buffer old-viewport
                               (rename-buffer (concat title " [viewport]") t))))
                         (message "egent-label: labeled %s" title)
                         (when (fboundp 'egent-sidebar-refresh)
                           (egent-sidebar-refresh)))))))))
      ;; Close stdin so the subprocess doesn't wait for piped input.
      (process-send-eof proc))))

(provide 'egent-label)
;;; egent-label.el ends here
