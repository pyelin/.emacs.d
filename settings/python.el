;;; Python

(flycheck-define-checker
    python-mypy ""
    :command ("mypy"
              "--ignore-missing-import"
              "--python-version" "3.6"
              source-original)
    :error-patterns
    ((error line-start (file-name) ":" line ": error:" (message) line-end))
    :modes python-mode)

(use-package anaconda-mode
  :config
  (setq python-shell-interpreter "python3")
  (setq flycheck-python-flake8-executable "python3")
  (add-to-list 'flycheck-checkers 'python-mypy t)
  (setq-default flycheck-disabled-checkers
    (append flycheck-disabled-checkers
      '(python-pycompile
        python-pylint)))
  (add-hook 'python-mode-hook 'anaconda-mode))
