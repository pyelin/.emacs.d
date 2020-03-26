(setenv "TSSERVER_LOG_FILE" "/tmp/tsserver.log")
(use-package lsp-mode
  :init (setq lsp-keymap-prefix "C-c C-l")
  :hook
  (
    (js2-mode . lsp)
    (sql-mode . lsp)
  )
  :commands lsp)

(use-package company-lsp :commands company-lsp)
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
