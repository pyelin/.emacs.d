;;; package --- Config for javascript
;;; Code:
;;; Commentary:

(use-package js2-mode
  :mode (rx ".js" eos)
  :custom
  (js-indent-level 2)
  :config
  (add-hook 'js2-mode-hook
    (lambda ()
      (push '("function" . ?ƒ) prettify-symbols-alist)
      (prettify-symbols-mode)
      (highlight-indent-guides-mode)
      (hs-minor-mode))))

(use-package json-mode
  :mode (rx ".json" eos))

(use-package web-mode
  :ensure t
  :mode (rx (or ".html" ".jsx" ".tsx") eos)
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-block-padding 2)
  (web-mode-comment-style 2)

  (web-mode-enable-css-colorization t)
  (web-mode-enable-auto-pairing t)
  (web-mode-enable-comment-keywords t)
  (web-mode-enable-current-element-highlight t)
	(web-mode-enable-auto-indentation nil))

(use-package typescript-mode
  :mode (rx ".ts" eos)
  :ensure t
  :custom
  (typescript-indent-level 2)
  :config
  (add-hook 'typescript-mode #'subword-mode))

(use-package css-mode
  :mode (rx ".css" eos)
  :custom
  (css-indent-offset 2))

(use-package js-comint
  :config
  (cond ((window-p) (setq js-comint-program-command "C:/Program Files/nodejs/node.exe")))
  (define-key js-mode-map (kbd "C-x C-e") (lambda () (interactive) (js-clear)(js-send-region))))
