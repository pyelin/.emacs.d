;;; markdown.el --- -*- lexical-binding: t; -*-
(use-package md-mode
  :straight (:type git :host github :repo "yibie/md-mode")
  :mode ("\\.md\\'" . md-mode))
