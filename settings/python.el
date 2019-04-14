;;; Python

(use-package python-mode
  :mode ("\\.py\\'" . python-mode)
  :config (add-hook 'python-mode-hook 'highlight-indent-guides-mode))
