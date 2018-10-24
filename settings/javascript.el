(use-package js2-mode
  :mode ("\\.js\\'" . js2-mode)
  :config
  (add-hook 'js2-mode-hook
          (lambda ()
            (push '("function" . ?ƒ) prettify-symbols-alist)
            (prettify-symbols-mode)))
  ;; disable jshint since we prefer eslint checking
  ;; run "$ npm install -g eslint babel-eslint eslint-plugin-react" and other dependencies in console
  ;; check if eslist is working by running M-x: "flycheck-verify-setup" and console: "eslint --print-config ."
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint)))
  (add-hook 'flycheck-mode-hook #'use-eslint-from-node-modules))

(use-package web-mode
  :mode ("\\.jsx\\'" . web-mode)
  :config
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
  (add-hook 'web-mode-hook
          (lambda ()
            (push '("function" . ?ƒ) prettify-symbols-alist)
            (prettify-symbols-mode)
	    ;; fix case-sensitive autocomplete in web-mode
	    (add-to-list 'company-dabbrev-code-modes 'web-mode)))
  ;; use eslint with web-mode for jsx files
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  ;; disable json-jsonlist checking for json files
  (setq-default flycheck-disabled-checkers
		(append flycheck-disabled-checkers
			'(json-jsonlist)))
  ;; for better jsx syntax-highlighting in web-mode
  ;; - courtesy of Patrick @halbtuerke
  (defadvice web-mode-highlight-part (around tweak-jsx activate)
    (if (equal web-mode-content-type "jsx")
	(let ((web-mode-enable-part-face nil))
	  ad-do-it)
      ad-do-it)))

(use-package json-mode
  :mode ("\\.json\\'" . json-mode)
  :config
  (add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode)))

;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
(defun use-eslint-from-node-modules ()
  "Use local eslint from node_modules before global."
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))
