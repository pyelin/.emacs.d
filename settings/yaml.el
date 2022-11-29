  (use-package outline-magic :ensure t)
  (use-package yaml-mode
    :ensure t
    :config
    (progn
      (add-hook 'yaml-mode-hook 'leoc/yaml-outline-hook)

      ;; Customize folding markers
      (set-display-table-slot
       standard-display-table
       'selective-display
       (let ((face-offset (* (face-id 'shadow) (lsh 1 22))))
         (vconcat (mapcar (lambda (c) (+ face-offset c)) " [+]"))))

      (defun leoc/yaml-outline-level ()
        (s-count-matches "\\([ ]\\{2\\}\\)" (match-string 0)))

      (defun leoc/yaml-outline-hook ()
        (interactive)
        (setq outline-regexp
              (rx
               (seq
                bol
                (group (zero-or-more "  ")
                       (or (group
                            (seq (or (seq "\"" (*? (not (in "\"" "\n"))) "\"")
                                     (seq "'" (*? (not (in "'" "\n"))) "'")
                                     (*? (not (in ":" "\n"))))
                                 ":"
                                 (?? (seq
                                      (*? " ")
                                      (or (seq "&" (one-or-more nonl))
                                          (seq ">-")
                                          (seq "|"))
                                      eol))))
                           (group (seq
                                   "- "
                                   (+ (not (in ":" "\n")))
                                   ":"
                                   (+ nonl)
                                   eol)))))))

        (setq outline-level 'leoc/yaml-outline-level)

        (outline-minor-mode t)
        (hide-body)
        (show-paren-mode 1)
        (define-key yaml-mode-map [tab] 'outline-cycle)
        (define-key outline-minor-mode-map [M-S-tab] 'indent-for-tab-command)
        (define-key outline-minor-mode-map [M-down] 'outline-move-subtree-down)
        (define-key outline-minor-mode-map [M-up] 'outline-move-subtree-up))))
