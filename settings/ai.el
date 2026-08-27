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
  ;; A single frame renders the busy indicator as static text rather than an
  ;; animation, so the header stops flickering while the agent works.
  (agent-shell-busy-indicator-frames '("Working…"))
  ;; The project root is sent with a trailing slash, but pi-acp matches session
  ;; cwds by string equality against pi's own session header, which has none, so
  ;; `session/list' comes back empty: the picker offers no past sessions and the
  ;; session title never arrives.  Harmless for the file paths this hook also sees.
  (agent-shell-path-resolver-function #'directory-file-name)
  :config
  (setopt agent-shell-agent-configs
    (list #'agent-shell-anthropic-make-claude-code-config
          #'agent-shell-cursor-make-agent-config
          #'agent-shell-pi-make-agent-config))
  (setopt agent-shell-anthropic-authentication
    (agent-shell-anthropic-make-authentication :login t))
  (setopt agent-shell-cursor-authentication
    (agent-shell-cursor-make-authentication :none t))
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
  ;; Tool-call commands are fenced as ```console, which resolves to a
  ;; nonexistent console-mode and so renders unhighlighted.  Alias it to
  ;; sh-mode for shell syntax highlighting in the command panel.
  (require 'sh-script)
  (add-to-list 'agent-shell-markdown-language-mapping '("console" . "sh")))

;; Replaces agent-shell-hq.  Developed in-tree for now, so no straight recipe.
;; `user-emacs-directory' is ~/.emacs.d (where straight keeps its clones), but
;; this config tree lives elsewhere, so derive the path from `settings-dir'
;; rather than letting `:load-path' resolve it against the wrong root.
(add-to-list 'load-path
  (expand-file-name "egent" (file-name-directory (directory-file-name settings-dir))))

(use-package egent
  :straight nil
  :commands (egent-sidebar-toggle egent-sidebar-focus egent-peek egent-resume egent-label)
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
  (egent-label-command '("claude" "-p" "--model" "haiku"))
  ;; Characters of buffer content (from the end) used as context for the title
  (egent-label-context-chars 2000)
  ;; Prompt template sent to the command (%s = buffer context)
  (egent-label-prompt
   "Reply with ONLY a terse 8-10 word title for this conversation, lowercase, no punctuation:\n\n%s")
  :bind
  ("C-c a h" . egent-sidebar-toggle)
  ("C-c a p" . egent-peek)
  ("C-c a r" . egent-resume))
