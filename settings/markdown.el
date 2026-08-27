;;; markdown.el --- -*- lexical-binding: t; -*-
(use-package markdown-ts-mode
  ;; Built-in since Emacs 31; `:ensure nil' is ignored under
  ;; `straight-use-package-by-default', so `:straight nil' is what stops
  ;; straight from installing the obsolete third-party package over it.
  :straight nil
  :mode ("\\.md\\'" "\\.mdx\\'" "\\.markdown\\'")
  :config
  (require 'markdown-ts-mode-x))
