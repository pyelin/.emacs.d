;;; package --- Config for javascript
;;; Code:
;;; Commentary:

(use-package js2-mode
  :mode ("\\.js\\'" . js2-mode)
  :config
  (add-hook 'js2-mode-hook
    (lambda ()
      (push '("function" . ?ƒ) prettify-symbols-alist)
      (prettify-symbols-mode)
      (highlight-indent-guides-mode)))
  ;; disable jshint since we prefer eslint checking
  ;; run "$ npm install -g eslint babel-eslint eslint-plugin-react" and other dependencies in console
  ;; check if eslist is working by running M-x: "flycheck-verify-setup" and console: "eslint --print-config ."
  (setq-default flycheck-disabled-checkers
    (append flycheck-disabled-checkers
      '(javascript-jshint))))

(use-package rjsx-mode
  :mode ("\\.*sx\\'" . rjsx-mode))

(use-package json-mode
  :mode ("\\.json\\'" . json-mode))


(provide 'javascript)
;;; javascript.el ends here
