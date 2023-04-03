(use-package flymake
  :straight nil
  :custom
  (flymake-fringe-indicator-position 'left-fringe))

(use-package eglot
  :custom
  (eglot-autoshutdown t)
  :hook
  (typescript-mode . eglot-ensure)
  (web-mode . eglot-ensure)
  (python-mode . eglot-ensure)
  (js2-mode . eglot-ensure)
  (move-mode . eglot-ensure)
  :init
  (put 'eglot-server-programs 'safe-local-variable 'listp)
  ;; for python, run pip3 install 'python-lsp-server[all]' pylsp-mypy
  :config
  (add-to-list 'eglot-server-programs '(web-mode . ("typescript-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs '(move-mode . ("move-analyzer")))
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

(use-package tree-sitter
  :hook
  (typescript-mode . tree-sitter-hl-mode)
  (web-mode . tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :after tree-sitter
  :defer nil
  :config
  (tree-sitter-require 'tsx)
  (add-to-list 'tree-sitter-major-mode-language-alist
    '(web-mode . tsx)))

(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t
  :hook
  (copilot-complete . copilot-clear-overlay)
  :config
  (defun pye/copilot-tab ()
    (interactive)
    (or (copilot-accept-completion)
      (indent-for-tab-command)))

  (define-key copilot-mode-map (kbd "<tab>") #'pye/copilot-tab))

(use-package gptel
  :config
  (setq gptel-api-key (getenv "OPENAI_API_KEY")))
