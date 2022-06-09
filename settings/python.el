;;; Python

(setq python-shell-interpreter "python3")

(use-package anaconda-mode
  :mode ("\\.py\\'" . python-mode)
  :config
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode))


;; (use-package company-anaconda
;;   :mode ("\\.py\\'" . python-mode)
;;   :config
;;   (eval-after-load "company"
;;     '(add-to-list 'company-backends 'company-anaconda)))
