;;; Python

(use-package elpy
  :ensure t
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable)
  :config (add-hook 'python-mode-hook 'highlight-indent-guides-mode))
