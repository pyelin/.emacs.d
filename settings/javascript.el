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
  :mode (rx (or ".html" ".jsx") eos)
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
  :after tree-sitter
  :config
  ;; we choose this instead of tsx-mode so that eglot can automatically figure out language for server
  ;; see https://github.com/joaotavora/eglot/issues/624 and https://github.com/joaotavora/eglot#handling-quirky-servers
  (define-derived-mode typescriptreact-mode typescript-mode
    "TypeScript TSX")

  ;; use our derived mode for tsx files
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescriptreact-mode))
  ;; by default, typescript-mode is mapped to the treesitter typescript parser
  ;; use our derived mode to map both .tsx AND .ts -> typescriptreact-mode -> treesitter tsx
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescriptreact-mode . tsx)))

(use-package css-mode
  :mode (rx ".css" eos)
  :custom
  (css-indent-offset 2))

(use-package js-comint
  :config
  (when (window-p)
    (setq js-comint-program-command "C:/Program Files/nodejs/node.exe"))
  (define-key js-mode-map (kbd "C-x C-e")
    (lambda ()
      (interactive)
      (js-clear)
      (js-send-region))))
