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
  :init
  (put 'eglot-server-programs 'safe-local-variable 'listp)
  ;; for python, run pip3 install 'python-lsp-server[all]' pylsp-mypy
  :config
  (add-to-list
    'eglot-server-programs
    '(web-mode . ("typescript-language-server" "--stdio")))
  (add-to-list 'eglot-stay-out-of 'eldoc-documentation-strategy)
  (put 'eglot-error 'flymake-overlay-control nil)
  (put 'eglot-warning 'flymake-overlay-control nil)
  (advice-add 'project-kill-buffers :before #'pye/eglot-shutdown-project)
  :custom
  (eglot-ignored-server-capabilites '(:documentHighlightProvider))
  :preface
  (defun pye/eglot-shutdown-project ()
    "Kill the LSP server for the current project if it exists."
    (when-let ((server (eglot-current-server)))
      (eglot-shutdown server))))
