(use-package markdown-mode
  :mode (("changelog\\.md\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  :config
  (defun pye/markdown-insert-file-link ()
    "Prompt for a file in the current directory and insert a Markdown link."
    (interactive)
    (let* ((default-directory (file-name-directory (or (buffer-file-name) default-directory)))
           (file (read-file-name "Select file: " default-directory))
           (filename (file-relative-name file default-directory))
           (link-text (read-string "Link text: ")))
      (insert (format "[%s](%s)" link-text filename))))
  (define-key markdown-mode-map (kbd "C-c C-l") 'pye/markdown-insert-file-link))
