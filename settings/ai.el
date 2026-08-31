;;; ai.el --- -*- lexical-binding: t; -*-
(use-package flymake
  :straight nil
  :custom
  (flymake-fringe-indicator-position 'left-fringe))

(use-package eglot
  :custom
  (eglot-autoshutdown t)
  :hook
  (typescript-mode . eglot-ensure)
  (typescript-ts-mode . eglot-ensure)
  (tsx-ts-mode . eglot-ensure)
  (web-mode . eglot-ensure)
  (python-ts-mode . eglot-ensure)
  (js2-mode . eglot-ensure)
  (move-mode . eglot-ensure)
  (rust-ts-mode . eglot-ensure)
  :init
  (put 'eglot-server-programs 'safe-local-variable 'listp)
  :config
  (add-to-list 'eglot-server-programs '(web-mode . ("typescript-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs '(move-mode . ("move-analyzer")))

  ;; python
  (add-to-list 'eglot-server-programs '(python-base-mode . ("ruff" "server")))
  (add-hook 'python-base-mode-hook
    (lambda ()
      (eglot-ensure)
      (add-hook 'after-save-hook 'eglot-format nil t)))

  (add-to-list 'eglot-stay-out-of 'eldoc-documentation-strategy)
  (put 'eglot-error 'flymake-overlay-control nil)
  (put 'eglot-warning 'flymake-overlay-control nil)
  (advice-add 'project-kill-buffers :before #'pye/eglot-shutdown-project)

  ;; Move packages are nested in sub-directories so look for the relevant Move.toml
  ;; https://github.com/amnn/move-mode#eglot
  (defun pye/move-lsp-project-root (dir)
    (and-let* (((boundp 'eglot-lsp-context))
                (eglot-lsp-context)
                (override (locate-dominating-file dir "Move.toml")))
      (cons 'Move.toml override)))

  (add-hook 'project-find-functions #'pye/move-lsp-project-root)
  (cl-defmethod project-root ((project (head Move.toml)))
    (cdr project))

  :custom
  (eglot-ignored-server-capabilites '(:documentHighlightProvider))
  :preface
  (defun pye/eglot-shutdown-project ()
    "Kill the LSP server for the current project if it exists."
    (when-let ((server (eglot-current-server)))
      (eglot-shutdown server))))


(use-package pi-coding-agent
  :ensure t
  :init (defalias 'pi 'pi-coding-agent))

;;;; ACP coding agents
;; Each agent talks the Agent Client Protocol through an external adapter that
;; must be on PATH:
;;   claude-code  npm install -g @zed-industries/claude-agent-acp
;;   cursor       Cursor CLI's own `agent acp' (cursor.com/install)
;;   pi           npm install -g pi-acp
;; Auth lives outside Emacs: `claude' uses the subscription login, Cursor uses
;; whatever `agent login' left behind.
;;
;; pi ships its own node under a version-stamped directory that only `.zshrc'
;; puts on PATH, so a daemon started before that export existed never inherits
;; it and `pi-acp' looks missing. Resolve the directory instead of trusting PATH.
(let ((bin (car (last (file-expand-wildcards
                        (expand-file-name "~/.local/share/pi-node/node-*/bin"))))))
  (when (and bin (not (member bin exec-path)))
    (add-to-list 'exec-path bin)
    (setenv "PATH" (concat bin path-separator (getenv "PATH")))))

(use-package agent-shell
  :straight (:host github :repo "xenodium/agent-shell")
  :commands (agent-shell
              agent-shell-new-shell
              agent-shell-send-dwim
              agent-shell-anthropic-start-claude-code
              agent-shell-cursor-start-agent
              agent-shell-pi-start-agent)
  :custom
  ;; Follow the agent's reasoning as it streams instead of having to unfold it.
  ;; The surrounding activity group still uses `latest', so a finished thought
  ;; tucks itself away once the agent moves on.
  (agent-shell-thought-process-expand-by-default t)
  ;; A single frame renders the busy indicator as a static glyph rather than an
  ;; animation, so the header stops flickering while the agent works.
  (agent-shell-busy-indicator-frames '("ε=┌(＞o＜)┘"))
  ;; The project root is sent with a trailing slash, but pi-acp matches session
  ;; cwds by string equality against pi's own session header, which has none, so
  ;; `session/list' comes back empty: the picker offers no past sessions and the
  ;; session title never arrives.  Harmless for the file paths this hook also sees.
  (agent-shell-path-resolver-function #'directory-file-name)
  ;; This Emacs is built without D-Bus, so `system-sleep' has no back end on
  ;; GNU/Linux and warns as soon as it is loaded.  Nothing here can block idle
  ;; sleep anyway, so don't ask agent-shell to try.
  (agent-shell-inhibit-system-sleep nil)
  :config
  ;; The machine-local default model lives in ~/.zshrc (exported there as
  ;; PI_ACP_PI_COMMAND), because ~/.pi/agent/settings.json is repo-tracked.
  ;; `pi-acp' spawns the `pi' binary directly, so the `pi' shell function is
  ;; never consulted and the session would fall back to `defaultModel' from
  ;; settings.json.  Forward the variable only when the environment defines
  ;; it, so machines without the wrapper keep their own resolution.
  (when-let* ((pi-command (getenv "PI_ACP_PI_COMMAND")))
    (setopt agent-shell-pi-environment
      (agent-shell-make-environment-variables
        "PI_ACP_PI_COMMAND" pi-command)))
  (setopt agent-shell-agent-configs
    (list #'agent-shell-anthropic-make-claude-code-config
          #'agent-shell-cursor-make-agent-config
          #'agent-shell-pi-make-agent-config))
  ;; Keep the picker, but list pi first and offer it as the default choice.
  ;; A bare identifier would skip the prompt entirely; the `preselect' cons
  ;; only reorders and preselects.
  (setopt agent-shell-preferred-agent-config '(preselect . pi))
  (setopt agent-shell-anthropic-authentication
    (agent-shell-anthropic-make-authentication :login t))
  (setopt agent-shell-cursor-authentication
    (agent-shell-cursor-make-authentication :none t))
  ;; pi-acp advertises every authenticated model, even when pi's settings scope
  ;; model selection.  Keep agent-shell's picker consistent with pi's scope.
  (defun my/agent-shell--pi-enabled-models (root)
    "Return pi's effective `enabledModels' setting for ROOT."
    (require 'json)
    (cl-labels
        ((read-setting (file)
           (when (file-readable-p file)
             (condition-case nil
                 (with-temp-buffer
                   (insert-file-contents file)
                   (let* ((settings
                           (json-parse-buffer :object-type 'alist
                                              :array-type 'list
                                              :null-object nil
                                              :false-object nil))
                          (entry (assq 'enabledModels settings)))
                     (when entry (cons t (cdr entry)))))
               (error nil)))))
      (let* ((agent-dir (expand-file-name
                         (or (getenv "PI_CODING_AGENT_DIR") "~/.pi/agent")))
             (global (read-setting (expand-file-name "settings.json" agent-dir)))
             (project (read-setting
                       (expand-file-name ".pi/settings.json" root))))
        (cdr (or project global)))))
  (defun my/agent-shell--pi-model-pattern (pattern)
    "Return PATTERN without a pi thinking-level suffix."
    (if (string-match
         "\\`\\(.*\\):\\(?:off\\|minimal\\|low\\|medium\\|high\\|xhigh\\|max\\)\\'"
         pattern)
        (match-string 1 pattern)
      pattern))
  (defun my/agent-shell--pi-model-matches-p (pattern model-id)
    "Return non-nil when pi model PATTERN matches MODEL-ID."
    (let* ((pattern (my/agent-shell--pi-model-pattern pattern))
           (candidate (if (string-search "/" pattern)
                          model-id
                        (string-join (cdr (split-string model-id "/")) "/"))))
      (string-match-p (wildcard-to-regexp pattern) candidate)))
  (defun my/agent-shell--scope-pi-models (&rest _args)
    "Restrict the current pi shell's model choices to its configured scope."
    (when (eq (map-nested-elt (agent-shell--state)
                              '(:agent-config :identifier))
              'pi)
      (let ((patterns (my/agent-shell--pi-enabled-models (agent-shell-cwd))))
        ;; An empty scope means all models in pi.
        (when patterns
          (let* ((state (agent-shell--state))
                 (model-option
                  (agent-shell--config-option-by-category state "model"))
                 (options (map-elt model-option :options))
                 (models (map-nested-elt state '(:session :models)))
                 (ordered-options nil)
                 (ordered-models nil))
            (dolist (pattern patterns)
              (dolist (option options)
                (when (and
                       (my/agent-shell--pi-model-matches-p
                        pattern (map-elt option :value))
                       (not (seq-find
                             (lambda (item)
                               (equal (map-elt item :value)
                                      (map-elt option :value)))
                             ordered-options)))
                  (setq ordered-options (append ordered-options (list option)))))
              (dolist (model models)
                (when (and
                       (my/agent-shell--pi-model-matches-p
                        pattern (map-elt model :model-id))
                       (not (seq-find
                             (lambda (item)
                               (equal (map-elt item :model-id)
                                      (map-elt model :model-id)))
                             ordered-models)))
                  (setq ordered-models (append ordered-models (list model))))))
            (when model-option
              (setf (map-elt model-option :options) ordered-options))
            (let ((session (map-elt state :session)))
              (setf (map-elt session :models) ordered-models)
              (setf (map-elt state :session) session)))))))
  (advice-add 'agent-shell--save-config-options
    :after #'my/agent-shell--scope-pi-models)
  ;; Skip the ASCII-art logos, keep the plain welcome messages.
  (advice-add 'agent-shell-anthropic--claude-code-welcome-message
    :override #'shell-maker-welcome-message)
  (advice-add 'agent-shell-pi--welcome-message
    :override #'shell-maker-welcome-message)
  (advice-add 'agent-shell-cursor--welcome-message
    :override #'shell-maker-welcome-message)
  ;; Enable mouse support (scrolling etc.) in terminal Emacs.
  (unless (display-graphic-p)
    (xterm-mouse-mode 1))
  ;; Refresh magit buffers as the agent changes files (debounced), plus
  ;; a final refresh when the turn completes.
  (defvar my/agent-shell--magit-refresh-timer nil)
  (defun my/agent-shell--magit-refresh-soon (shell-buffer)
    (when (timerp my/agent-shell--magit-refresh-timer)
      (cancel-timer my/agent-shell--magit-refresh-timer))
    (setq my/agent-shell--magit-refresh-timer
      (run-with-timer 0.5 nil
        (lambda ()
          (when (and (buffer-live-p shell-buffer)
                     (fboundp 'magit-refresh-all))
            (with-current-buffer shell-buffer
              (magit-refresh-all)))))))
  (defun my/agent-shell-magit-refresh-setup ()
    (let ((shell-buffer (current-buffer)))
      (dolist (event '(file-write tool-call-update turn-complete))
        (agent-shell-subscribe-to
          :shell-buffer shell-buffer
          :event event
          :on-event (lambda (_event)
                      (my/agent-shell--magit-refresh-soon shell-buffer))))))
  (add-hook 'agent-shell-mode-hook #'my/agent-shell-magit-refresh-setup)
  ;; pi-acp advertises the thinking level twice: as ACP session modes and as a
  ;; thought_level config option, so the header showed "Thinking: high" twice.
  ;; Drop the mode segment only when it duplicates the thought level, so real
  ;; session modes (Claude Code's Accept Edits, etc.) still show.
  (defun my/agent-shell--drop-duplicate-mode-name (fn state)
    (let ((mode-name (funcall fn state)))
      (unless (equal mode-name (agent-shell-get-thought-level-name state))
        mode-name)))
  (advice-add 'agent-shell-get-mode-name
    :around #'my/agent-shell--drop-duplicate-mode-name)
  ;; An attached image is previewed by hanging the image object off the
  ;; link's `display' property, which this build cannot make: it carries
  ;; no image support, so the link shows as bare text.  A propertized
  ;; string is just as good a `display' value, so hand it the same chafa
  ;; rendering `pye/image-preview' shows, and the preview lands inline.
  (defun pye/agent-shell-text-image (fn &rest args)
    "Return FN's image for ARGS, or a text rendering when there is none."
    (or (apply fn args)
        (when (fboundp 'pye/image-render-to-string)
          (let ((columns (max 20 (min 60 (- (window-body-width) 8)))))
            (pye/image-render-to-string (plist-get args :file-path)
                                        columns
                                        (max 8 (/ columns 3)))))))
  (advice-add 'agent-shell--load-image :around #'pye/agent-shell-text-image)
  ;; Tool-call commands are fenced as ```console, which resolves to a
  ;; nonexistent console-mode and so renders unhighlighted.  Alias it to
  ;; sh-mode for shell syntax highlighting in the command panel.
  (require 'sh-script)
  (add-to-list 'agent-shell-markdown-language-mapping '("console" . "sh"))

  ;; allow new line
  (with-eval-after-load 'agent-shell
    (define-key agent-shell-mode-map (kbd "RET") 'newline)
    (define-key agent-shell-mode-map (kbd "C-c C-c") 'shell-maker-submit)
    (define-key agent-shell-mode-map (kbd "C-c C-k") 'agent-shell-interrupt)))

;; Replaces agent-shell-hq.  Developed in-tree for now, so no straight recipe.
;; `user-emacs-directory' is ~/.emacs.d (where straight keeps its clones), but
;; this config tree lives elsewhere, so derive the path from `settings-dir'
;; rather than letting `:load-path' resolve it against the wrong root.
(add-to-list 'load-path
  (expand-file-name "egent" (file-name-directory (directory-file-name settings-dir))))

(use-package egent
  :straight nil
  :commands (egent-sidebar-toggle egent-sidebar-focus egent-peek egent-resume
                                  egent-name-session)
  :custom
  ;; Width of the sidebar listing sessions (columns)
  (egent-sidebar-width 50)
  ;; Ask each project's agents for resumable sessions when the sidebar opens
  (egent-sidebar-auto-fetch-sessions t)
  ;; Where the peek posframe is anchored: top, bottom, left, right
  (egent-peek-position 'right)
  ;; Width of the peek posframe (columns)
  (egent-peek-width 52)
  ;; Maximum height of the peek posframe (rows)
  (egent-peek-height 60)
  ;; CLI command that receives the prompt as its final argument
  (egent-session-name-command '("claude" "-p" "--model" "haiku"))
  ;; Characters of buffer content (from the end) used as context for the title
  (egent-session-name-context-chars 2000)
  ;; Prompt template sent to the command (%s = buffer context)
  (egent-session-name-prompt
   "Reply with ONLY a terse 8-10 word title for this conversation, lowercase, no punctuation:\n\n%s")
  :bind
  ("C-c a h" . egent-sidebar-toggle)
  ("C-c a p" . egent-peek)
  ("C-c a r" . egent-resume))

;; What a session is about only shows in the header line, which the graphical
;; style draws as an image and the viewport does not carry at all, so the mode
;; line says it too.  Viewport buffers resolve to their shell, so a session
;; reads the same from either side.
(defvar my/egent-modeline-name-width 40
  "Columns the session name is allowed in the mode line.")

(with-eval-after-load 'egent
  (defun my/egent-shell-buffer (buffer)
    "Return the agent shell BUFFER belongs to, or nil when it is not one.
A viewport resolves to the shell it was opened from, so a session reads
the same from either side.  Strict about what counts, unlike
`agent-shell--shell-buffer', which answers with the project's shell for
any buffer at all."
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (cond ((derived-mode-p 'agent-shell-mode) buffer)
              ((derived-mode-p 'agent-shell-viewport-view-mode
                               'agent-shell-viewport-edit-mode)
               (agent-shell-viewport--shell-buffer buffer))))))

  (defun my/egent-session-name (buffer)
    "Return the name of the session BUFFER shows, or the title standing in."
    (when-let* ((shell-buffer (my/egent-shell-buffer buffer)))
      (or (egent-buffer-session-name shell-buffer)
          (egent-nonempty
           (egent-one-line (egent-buffer-session-title shell-buffer))))))

  (doom-modeline-def-segment egent-session-name
    "Display the name of the current egent session."
    (when-let* ((name (my/egent-session-name (current-buffer))))
      (concat (doom-modeline-spc)
              (propertize (egent-truncate name my/egent-modeline-name-width)
                          'face 'doom-modeline-info
                          'help-echo name))))
  ;; Adding is not idempotent: it inserts another copy of the segment
  ;; every time this file is evaluated, and a re-eval runs the whole
  ;; `with-eval-after-load' body again.  Drop any earlier copy first.
  (doom-modeline-remove-segment 'egent-session-name)
  (doom-modeline-add-segment
   'egent-session-name 'buffer-info :after 'main)

  ;; pi's TUI footer (token totals, cache-hit rate, cost, context fill),
  ;; derived from the session file: pi-acp does not forward usage over
  ;; ACP, so agent-shell's own usage readouts stay empty for pi shells.
  (doom-modeline-def-segment egent-usage
    "Display pi session usage for the current agent shell."
    (when-let* ((shell-buffer (my/egent-shell-buffer (current-buffer)))
                ((fboundp 'egent-usage-string))
                (usage (egent-usage-string shell-buffer)))
      (concat (doom-modeline-spc)
              ;; The mode line parses %-constructs even in segment
              ;; strings, and drops ones it doesn't know (%/, "% ").
              (propertize (replace-regexp-in-string "%" "%%" usage)
                          'help-echo
                          "pi session usage: ↑in ↓out R/W cache, CH cache-hit %, cost, context%"))))
  (doom-modeline-remove-segment 'egent-usage)
  (doom-modeline-add-segment
   'egent-usage 'selection-info :after 'main))

;; The buffer picker lists shells by buffer name, which only says which order
;; they were opened in.  Annotating them with the session name puts what each
;; one is about beside it, where `consult-buffer' and `switch-to-buffer' both
;; read it.
(with-eval-after-load 'marginalia
  (defun my/marginalia-annotate-buffer (candidate)
    "Annotate buffer CANDIDATE, naming the agent session it is showing."
    (if-let* ((buffer (get-buffer candidate))
              ((buffer-live-p buffer))
              ((fboundp 'my/egent-session-name))
              (name (my/egent-session-name buffer)))
        (marginalia--fields
         (name :truncate 0.4 :face 'marginalia-value)
         ((marginalia--buffer-status buffer))
         ((marginalia--buffer-file buffer)
          :truncate -0.5 :face 'marginalia-file-name))
      (marginalia-annotate-buffer candidate)))

  ;; Ahead of the stock entry, which stays reachable through
  ;; `marginalia-cycle'.
  (add-to-list 'marginalia-annotators
               '(buffer my/marginalia-annotate-buffer
                        marginalia-annotate-buffer builtin none)))
