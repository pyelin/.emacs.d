(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-l")
  (setq lsp-prefer-flymake nil)
  (setq lsp-enable-symbol-highlighting t)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-headerline-breadcrumb-enable nil)

  :hook
  ((python-mode . lsp-deferred)
    (haskell-mode . lsp-deferred)
    (sql-mode . lsp-deferred))
  :commands lsp)

(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs)
(use-package lsp-haskell)


;; typescript using tide
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))

  (eldoc-mode +1)
  (tide-hl-identifier-mode +1))

(add-hook 'web-mode-hook
  (lambda ()
    (when (string-match-p "\\(ts\\|tsx\\)$" (file-name-extension buffer-file-name))
      (setup-tide-mode))))

(flycheck-add-mode 'typescript-tslint 'web-mode)
