(use-package haskell-mode
  :mode ("\\.hs\\'" . haskell-mode)
  :config
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode))
