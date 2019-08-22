;;; Python

(use-package elpy
  :ensure t
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable)
  :config
  (setq elpy-rpc-python-command "python3")
  (setq python-shell-interpreter "python3")
  (setq python-indent-guess-indent-offset nil)
  (setq elpy-modules '(elpy-module-sane-defaults
                        elpy-module-company
                        elpy-module-highlight-indentation))
  (setq flycheck-python-pycompile-executable "python3")
  (setq flycheck-python-pylint-executable "python3")
  (setq flycheck-python-flake8-executable "python3"))
