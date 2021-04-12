(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-l")

  :hook
  ((js2-mode . lsp-deferred)
   (python-mode . lsp-deferred)
   (haskell-mode . lsp-deferred))
  
  :commands lsp)

(use-package lsp-haskell)
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
