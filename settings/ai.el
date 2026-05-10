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

(use-package agent-shell
  :straight (:host github :repo "xenodium/agent-shell")
  :ensure t
  :ensure-system-package
  ;; Add agent installation configs here
  ((claude . "brew install claude-code")
    (claude-agent-acp . "npm install -g @zed-industries/claude-agent-acp"))
  :custom
  (agent-shell-anthropic-authentication
    (agent-shell-anthropic-make-authentication :login t))
  (agent-shell-google-authentication
      (agent-shell-google-make-authentication :login t)))

(use-package agent-shell-sidebar
  :after agent-shell
  :straight (:host github :repo "cmacrae/agent-shell-sidebar")
  :custom
  (agent-shell-sidebar-width "25%")
  (agent-shell-sidebar-minimum-width 70)
  (agent-shell-sidebar-maximum-width "40%")
  (agent-shell-sidebar-position 'left)
  (agent-shell-sidebar-locked t)
  (agent-shell-sidebar-default-config
    (agent-shell-anthropic-make-claude-code-config)))

(use-package magit-gptcommit
  :straight t
  :demand t
  :after magit
  :bind (:map git-commit-mode-map
          ("C-c C-g" . magit-gptcommit-commit-accept))
  :config
  (require 'llm-claude)
  (setq magit-gptcommit-llm-provider
        (make-llm-claude :key (getenv "ANTHROPIC_API_KEY")))
  ;; Enable magit-gptcommit-mode to watch staged changes and generate commit message automatically in magit status buffer
  ;; This mode is optional, you can also use `magit-gptcommit-generate' to generate commit message manually
  ;; `magit-gptcommit-generate' should only execute on magit status buffer currently
  (magit-gptcommit-mode 1)

  ;; Add gptcommit transient commands to `magit-commit'
  ;; Eval (transient-remove-suffix 'magit-commit '(1 -1)) to remove gptcommit transient commands
  (magit-gptcommit-status-buffer-setup))
