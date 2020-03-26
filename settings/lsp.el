(setenv "TSSERVER_LOG_FILE" "/tmp/tsserver.log")
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c C-l")
  (setq lsp-prefer-flymake nil)
  (setq lsp-pyls-plugins-pycodestyle-enabled nil)
  :hook
  (
    (js2-mode . lsp)
    (sql-mode . lsp)
    (python-mode . lsp)
  )
  :commands lsp)

(use-package company-lsp :commands company-lsp)
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
