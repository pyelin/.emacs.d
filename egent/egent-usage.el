;;; egent-usage.el --- Pi session usage readouts for egent  -*- lexical-binding: t -*-

;; Copyright (C) 2026

;; Author: Sreenivas Venkobarao
;; Package-Requires: ((emacs "29.1") (agent-shell "0.66.1"))

;;; Commentary:

;; pi's TUI footer — token totals, cache-hit rate, cost, context fill —
;; is computed from the session file pi writes under
;; ~/.pi/agent/sessions/--<cwd>--/.  pi-acp never forwards any of it
;; over ACP, so `agent-shell''s own usage readouts stay empty for pi
;; shells.  This file re-derives the same numbers from the session file
;; and exposes them as `egent-usage-string' for a mode line or header
;; line.
;;
;; Parsing is incremental: a file is read fully once and later reads
;; only consume what was appended, so a busy session costs one short
;; read per change rather than a full rescan.

;;; Code:

(require 'egent-core)
(require 'json)
(require 'map)

;;;; Customization

(defcustom egent-usage-include-model nil
  "Whether the usage string ends with the model and thinking level.
`agent-shell' already shows both in its header, so the default leaves
them out of an already crowded mode line."
  :type 'boolean
  :group 'egent)

(defcustom egent-usage-retry-interval 5
  "Seconds before looking again for a session file that was missing.
A session has no file until pi first persists it, so a shell that just
started reports nothing until then."
  :type 'number
  :group 'egent)

;;;; State

(defvar egent-usage--files (make-hash-table :test 'equal)
  "Maps (SESSION-ID . CWD) to (FILE . CHECKED-AT).
FILE is nil when the lookup found nothing, in which case CHECKED-AT
throttles retries to `egent-usage-retry-interval'.")

(defvar egent-usage--states (make-hash-table :test 'equal)
  "Maps session FILE to its incremental parse state.")

(defvar egent-usage--models nil
  "Hash mapping \"provider\\0id\" to (CONTEXT-WINDOW . REASONING-P).
Nil when the models store has not been read yet or could not be read.")

(defvar egent-usage--models-mtime nil
  "Modification time of the models store `egent-usage--models' reflects.")

;;;; Locating the session file

(defun egent-usage--agent-dir ()
  "Return pi's agent directory."
  (expand-file-name (or (getenv "PI_CODING_AGENT_DIR") "~/.pi/agent")))

(defun egent-usage--session-dir (cwd)
  "Return the directory pi stores CWD's sessions in.
Mirrors session-manager.ts: resolve symlinks, drop the leading
separator, turn the remaining separators and colons into dashes, and
wrap the result in \"--\"."
  (let* ((resolved (file-truename (directory-file-name cwd)))
         (path (replace-regexp-in-string
                "[/\\:]" "-" (string-remove-prefix "/" resolved))))
    (expand-file-name (concat "--" path "--")
                      (expand-file-name "sessions" (egent-usage--agent-dir)))))

(defun egent-usage--session-file (buffer)
  "Return pi's session file for the session BUFFER shows, or nil.
Only pi shells have a session file to read; other agents report usage
over ACP instead.  The file name ends in the session id, which is the
same id pi-acp reports as the ACP session id."
  (when (and (buffer-live-p buffer)
             (eq (egent-buffer-agent-identifier buffer) 'pi))
    (let ((id (egent-buffer-session-id buffer)))
      (when id
        (let* ((cwd (with-current-buffer buffer (agent-shell-cwd)))
               (key (cons id cwd))
               (cached (gethash key egent-usage--files)))
          (if (and cached (or (car cached)
                              (< (- (float-time) (cdr cached))
                                 egent-usage-retry-interval)))
              (car cached)
            (let ((file (car (file-expand-wildcards
                              (expand-file-name (format "*_%s.jsonl" id)
                                                (egent-usage--session-dir cwd))))))
              (puthash key (cons file (float-time)) egent-usage--files)
              file)))))))

;;;; Incremental parsing

(defun egent-usage--new-state ()
  "Return a fresh parse state.
:offset is the byte position parsed up to.  :totals is a vector of
[input output cache-read cache-write cost] accumulated across the whole
session, as pi's footer does.  :context-tokens is the latest assistant
message's total, or nil after a compaction until the next response
re-establishes it."
  (list :offset 0
        :totals (vector 0 0 0 0 0.0)
        :hit-rate nil
        :context-tokens nil
        :context-stale nil
        :model nil
        :msg-model nil
        :thinking nil))

(defun egent-usage--add-usage (totals usage)
  "Add USAGE, a session file usage object, into the TOTALS vector."
  (when usage
    (aset totals 0 (+ (aref totals 0) (or (alist-get 'input usage) 0)))
    (aset totals 1 (+ (aref totals 1) (or (alist-get 'output usage) 0)))
    (aset totals 2 (+ (aref totals 2) (or (alist-get 'cacheRead usage) 0)))
    (aset totals 3 (+ (aref totals 3) (or (alist-get 'cacheWrite usage) 0)))
    (aset totals 4 (+ (aref totals 4)
                      (or (map-nested-elt usage '(cost total)) 0)))))

(defun egent-usage--parse-line (state line)
  "Fold one session file LINE into STATE, ignoring unparseable lines."
  (condition-case nil
      (let* ((entry (json-parse-string line :object-type 'alist
                                            :array-type 'list
                                            :null-object nil
                                            :false-object nil))
             (type (alist-get 'type entry)))
        (cond
         ((equal type "message")
          (let* ((msg (alist-get 'message entry))
                 (role (alist-get 'role msg))
                 (usage (alist-get 'usage msg)))
            (cond
             ((equal role "assistant")
              (egent-usage--add-usage (plist-get state :totals) usage)
              (plist-put state :msg-model
                         (cons (alist-get 'provider msg)
                               (alist-get 'model msg)))
              (when usage
                ;; The hit rate is the latest message's, not a running
                ;; average: pi shows how well the context cache is
                ;; doing right now.
                (let ((prompt (+ (or (alist-get 'input usage) 0)
                                 (or (alist-get 'cacheRead usage) 0)
                                 (or (alist-get 'cacheWrite usage) 0))))
                  (plist-put state :hit-rate
                             (when (> prompt 0)
                               (* 100.0
                                  (/ (float (or (alist-get 'cacheRead usage) 0))
                                     prompt)))))
                ;; Aborted and errored responses never reached the
                ;; context, so they say nothing about its fill.
                (unless (member (alist-get 'stopReason msg)
                                '("aborted" "error"))
                  (let ((tokens (or (alist-get 'totalTokens usage)
                                    (+ (or (alist-get 'input usage) 0)
                                       (or (alist-get 'output usage) 0)
                                       (or (alist-get 'cacheRead usage) 0)
                                       (or (alist-get 'cacheWrite usage) 0)))))
                    (when (> tokens 0)
                      (plist-put state :context-tokens tokens)
                      (plist-put state :context-stale nil))))))
             ((equal role "toolResult")
              ;; Nested LLM work reported by tools, counted by pi.
              (egent-usage--add-usage (plist-get state :totals) usage)))))
         ((equal type "model_change")
          (plist-put state :model
                     (cons (alist-get 'provider entry)
                           (alist-get 'modelId entry))))
         ((equal type "thinking_level_change")
          (plist-put state :thinking (alist-get 'thinkingLevel entry)))
         ((member type '("branch_summary" "compaction"))
          ;; Summary generation is LLM work too, counted by pi.
          (egent-usage--add-usage (plist-get state :totals)
                                  (alist-get 'usage entry))
          ;; After a compaction the last assistant usage reflects the
          ;; pre-compaction context; the fill is unknown until the next
          ;; response.
          (when (equal type "compaction")
            (plist-put state :context-stale t)))))
      (error nil)))

(defun egent-usage--consume (state file start end)
  "Parse complete lines of FILE between bytes START and END into STATE.
A final partial line is left for the next pass: pi appends to the file
line by line, and a half-written line is only visible mid-write."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (let ((coding-system-for-read 'no-conversion))
      (insert-file-contents file nil start end))
    (goto-char (point-min))
    (let ((consumed 0)
          (bol (point-min)))
      (while (search-forward "\n" nil t)
        (let ((line (buffer-substring-no-properties bol (1- (point)))))
          ;; The buffer is unibyte, so a character position is a byte
          ;; position and a substring is the raw line.
          (unless (zerop (length line))
            (egent-usage--parse-line
             state (decode-coding-string line 'utf-8))))
        (setq consumed (point)
              bol (point)))
      ;; CONSUMED is 0 when the range holds no newline at all; the
      ;; offset must not move backwards in that case.
      (when (> consumed 0)
        (plist-put state :offset (+ start (1- consumed)))))))

(defun egent-usage--parse (file)
  "Return the parse state for FILE, consuming what was appended, or nil.
A file that shrank since it was last parsed was rewritten from scratch
and is reparsed from the beginning."
  (let* ((attrs (file-attributes file))
         (size (and attrs (file-attribute-size attrs))))
    (when size
      (let ((state (gethash file egent-usage--states)))
        (when (or (null state) (< size (plist-get state :offset)))
          (setq state (egent-usage--new-state))
          (puthash file state egent-usage--states))
        (when (> size (plist-get state :offset))
          (egent-usage--consume state file (plist-get state :offset) size))
        state))))

;;;; Model metadata

(defun egent-usage--models ()
  "Return a hash mapping \"provider\\0id\" to (CONTEXT-WINDOW . REASONING-P).
Read from pi's models store, re-read when the file changes; nil when
the store is unavailable."
  (let* ((file (expand-file-name "models-store.json" (egent-usage--agent-dir)))
         (mtime (and (file-readable-p file)
                     (file-attribute-modification-time
                      (file-attributes file)))))
    (when mtime
      (unless (and egent-usage--models
                   (equal mtime egent-usage--models-mtime))
        (let ((table (make-hash-table :test 'equal)))
          (condition-case nil
              ;; Not json-parse-file: it only exists in jansson builds,
              ;; and this keeps the byte-compiler from noticing.
              (with-temp-buffer
                (insert-file-contents file)
                (dolist (provider (json-parse-buffer :object-type 'alist
                                                     :array-type 'list
                                                     :null-object nil
                                                     :false-object nil))
                  (dolist (model (alist-get 'models (cdr provider)))
                    (puthash (concat (symbol-name (car provider)) "\0"
                                     (alist-get 'id model))
                             (cons (alist-get 'contextWindow model)
                                   (eq (alist-get 'reasoning model) t))
                             table))))
            (error nil))
          (setq egent-usage--models table
                egent-usage--models-mtime mtime)))
      egent-usage--models)))

;;;; Formatting

(defun egent-usage--format-tokens (count)
  "Format COUNT the way pi's footer does: 999, 1.2k, 12k, 1.2M, 12M."
  (cond
   ((< count 1000) (format "%d" count))
   ((< count 10000) (format "%.1fk" (/ count 1000.0)))
   ((< count 1000000) (format "%dk" (round count 1000)))
   ((< count 10000000) (format "%.1fM" (/ count 1000000.0)))
   (t (format "%dM" (round count 1000000)))))

(defun egent-usage-string (buffer)
  "Return pi's footer-style usage string for BUFFER's session, or nil.
BUFFER is an `agent-shell' buffer.  The string mirrors pi's TUI footer:
input/output/cache token totals, the latest cache-hit rate, cost, and
context fill against the model's window.  Nil when the session has
produced no usage yet, so callers can simply hide the readout."
  (when-let* ((file (egent-usage--session-file buffer))
              (state (egent-usage--parse file)))
    (let* ((totals (plist-get state :totals))
           (in (aref totals 0))
           (out (aref totals 1))
           (cache-read (aref totals 2))
           (cache-write (aref totals 3))
           (cost (aref totals 4))
           (model (or (plist-get state :model) (plist-get state :msg-model)))
           (models (egent-usage--models))
           (info (and model models
                      (gethash (concat (car model) "\0" (cdr model)) models)))
           (window (car-safe info))
           (tokens (and (not (plist-get state :context-stale))
                        (plist-get state :context-tokens)))
           (parts nil))
      (when (> in 0)
        (push (format "↑%s" (egent-usage--format-tokens in)) parts))
      (when (> out 0)
        (push (format "↓%s" (egent-usage--format-tokens out)) parts))
      (when (> cache-read 0)
        (push (format "R%s" (egent-usage--format-tokens cache-read)) parts))
      (when (> cache-write 0)
        (push (format "W%s" (egent-usage--format-tokens cache-write)) parts))
      (when (and (> (+ cache-read cache-write) 0) (plist-get state :hit-rate))
        (push (format "CH%.1f%%" (plist-get state :hit-rate)) parts))
      (when (> cost 0)
        (push (format "$%.3f" cost) parts))
      (when (and window (> window 0))
        (push (if tokens
                  (let ((pct (* 100.0 (/ (float tokens) window))))
                    (propertize
                     (format "%.1f%%/%s" pct
                             (egent-usage--format-tokens window))
                     'face (cond ((> pct 90) 'error)
                                 ((> pct 70) 'warning)
                                 (t nil))))
                (format "?/%s" (egent-usage--format-tokens window)))
              parts))
      (when (and egent-usage-include-model model)
        (push (concat (cdr model)
                      (let ((thinking (plist-get state :thinking)))
                        (if (and (cdr-safe info) thinking
                                 (not (equal thinking "off")))
                            (concat " • " thinking)
                          "")))
              parts))
      (when parts
        (mapconcat #'identity (nreverse parts) " ")))))

;;;###autoload
(defun egent-show-usage ()
  "Message the usage string for the current buffer's agent shell."
  (interactive)
  (message "%s" (or (egent-usage-string (current-buffer))
                    "No pi usage for this session")))

(provide 'egent-usage)
;;; egent-usage.el ends here
