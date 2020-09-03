;;; Python

(setq python-shell-interpreter "python3")
(setq flycheck-python-flake8-executable "python3")
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(python-pycompile python-pylint python-flake8)))

(use-package anaconda-mode
  :mode ("\\.py\\'" . python-mode)
  :config
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode))


(use-package company-anaconda
  :mode ("\\.py\\'" . python-mode)
  :config
  (eval-after-load "company"
    '(add-to-list 'company-backends 'company-anaconda)))
