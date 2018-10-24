(use-package markdown-mode
  :mode (("changelog\\.md\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))
