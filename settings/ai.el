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


(use-package monet
 :straight (:type git :host github :repo "stevemolitor/monet"))

;; install claude-code.el, using :depth 1 to reduce download size:
(use-package claude-code
  :straight (:type git :host github :repo "stevemolitor/claude-code.el" :branch "main" :depth 1
                   :files ("*.el" (:exclude "images/*")))
  :bind-keymap
  ("C-c c" . claude-code-command-map) ;; or your preferred key
  ;; Optionally define a repeat map so that "M" will cycle thru Claude auto-accept/plan/confirm modes after invoking claude-code-cycle-mode / C-c M.
  :bind
  (:repeat-map pye-claude-code-map ("M" . claude-code-cycle-mode))
  :custom
  (claude-code-terminal-backend 'ghostel)
  :config
  ;; optional IDE integration with Monet
  (add-hook 'claude-code-process-environment-functions #'monet-start-server-function)
  (monet-mode 1)

  (claude-code-mode))

(use-package llm
  :init
  (require 'llm-claude))

(use-package magit-gptcommit
  :straight t
  :demand t
  :after magit
  :bind (:map git-commit-mode-map
          ("C-c C-g" . magit-gptcommit-commit-accept))
  :custom
  (magit-gptcommit-llm-provider (make-llm-claude :key (getenv "ANTHROPIC_API_KEY")))

  :config
  ;; Enable magit-gptcommit-mode to watch staged changes and generate commit message automatically in magit status buffer
  ;; This mode is optional, you can also use `magit-gptcommit-generate' to generate commit message manually
  ;; `magit-gptcommit-generate' should only execute on magit status buffer currently
  (magit-gptcommit-mode 1)

  ;; Add gptcommit transient commands to `magit-commit'
  ;; Eval (transient-remove-suffix 'magit-commit '(1 -1)) to remove gptcommit transient commands
  (magit-gptcommit-status-buffer-setup))
