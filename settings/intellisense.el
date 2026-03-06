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

(use-package gptel
  :straight (:host github :repo "karthink/gptel")
  :config
  (setq gptel-model 'gemini-2.5-flash-lite)
  (setq gptel-backend (gptel-make-gemini "Gemini"
    :key (getenv "GEMINI_API_KEY")
    :stream t)))

(use-package gptel-magit
  :hook (magit-mode . gptel-magit-install)
  :custom
  (gptel-magit-commit-prompt gptel-magit-prompt-zed))

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

