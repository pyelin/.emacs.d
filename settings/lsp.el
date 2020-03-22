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

;;Increase the amount of data which Emacs reads from the
;;process. Again the emacs default is too low 4k considering that the
;;some of the language server responses are in 800k - 3M range.
(setq read-process-output-max (* 1024 1024)) ;; 1mb
