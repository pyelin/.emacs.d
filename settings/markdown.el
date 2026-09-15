;;; markdown.el --- -*- lexical-binding: t; -*-
(use-package textui
  :straight (:type git :host github :repo "yibie/textui"))

(use-package md-mode
  :straight (:type git :host github :repo "yibie/md-mode")
  :mode ("\\.md\\'" . md-mode))
