;;; Python

(use-package elpy
  :mode ("\\.py\\'" . python-mode)
  :init (with-eval-after-load 'python (elpy-enable))
  :config
  (add-hook 'python-mode-hook 'highlight-indent-guides-mode))
