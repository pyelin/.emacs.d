(use-package org
  :straight (:type built-in)
  :mode (("\\.org\\'" . org-mode)
          ("\\.org.txt\\'" . org-mode))
  :bind (:map org-mode-map
          ("C-`" . pye/org-babel-to-buffer))
  :defer t
  :config
  (setq org-image-actual-width nil)
  (setq org-log-done t)
  (setq org-todo-keywords '((sequence "TODO" "REVIEW" "ACCEPTED" "DONE")))
  (setq org-todo-keyword-faces
    '(("REVIEW" . (:foreground "hot pink"))
       ("ACCEPTED" . (:foreground "cyan"))))
  (setq org-tag-faces
   '(("paused"  . (:background "#C00000"))))
  (setq org-confirm-babel-evaluate nil)
  (setq org-babel-python-command "python3")
  (setq org-startup-truncated nil)
  (define-key global-map "\C-c l" 'org-store-link)
  (define-key global-map "\C-c a" 'org-agenda)
  (define-key org-mode-map (kbd "M-e") nil) ;; reserved for keybinding
  (define-key org-mode-map (kbd "<f8>") 'org-tree-slide-mode)
  (add-hook 'org-mode-hook
    (lambda ()
      (setq show-trailing-whitespace nil)
      ;; make the lines in the buffer wrap around the edges of the screen.
      ;; to press C-c q  or fill-paragraph ever again!
      (visual-line-mode)))
  (add-hook 'org-mode-hook 'flyspell-mode)
  (add-hook 'org-mode-hook 'yas-minor-mode)
  (add-hook 'org-mode-hook 'org-bullets-mode)
  ;; commented because of version mismatch error
  ;; (org-babel-do-load-languages 'org-babel-load-languages '((js . t)))
  ;; (org-babel-do-load-languages 'org-babel-load-languages '((python . t)))
  ;; (org-babel-do-load-languages 'org-babel-load-languages '((sql . t)))
  ;; (org-babel-do-load-languages 'org-babel-load-languages '((sqlite . t)))
  ;; (org-babel-do-load-languages 'org-babel-load-languages '((shell . t)))
  ;; (org-babel-do-load-languages 'org-babel-load-languages '((restclient . t)))
  ;; (org-babel-do-load-languages 'org-babel-load-languages '((ditaa . t)))
  )

(use-package ob-restclient
  :mode (("\\.org\\'" . org-mode)
          ("\\.org.txt\\'" . org-mode)))

(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(defun pye/org-babel-to-buffer ()
  "A function to efficiently feed babel code block result to a separate buffer"
  (interactive)
  (org-open-at-point)
  (org-babel-remove-result))

(use-package org-bullets)

(use-package org-super-agenda
  :defer t
  :config
  (setq org-super-agenda-groups
       '((:log t)  ; Automatically named "Log"
         (:name "Schedule"
                :time-grid t)
         (:name "Today"
                :scheduled today)
         (:habit t)
         (:name "Due today"
                :deadline today)
         (:name "Overdue"
           :deadline past
           :todo ("TODO"))
         (:name "Due soon"
                :deadline future)
         (:name "Unimportant"
                :todo ("SOMEDAY" "MAYBE" "CHECK" "TO-READ" "TO-WATCH")
                :order 100)
         (:name "Waiting..."
                :todo "WAITING"
                :order 98)
         (:name "Scheduled earlier"
                :scheduled past)))
  (org-super-agenda-mode))

(cond
    ((boundp 'pye/org-roam-directory)
      (use-package org-roam
        :ensure t
        :custom
        (org-roam-v2-ack t)
        (org-roam-node-display-template "${title:*} ${file:48}")
        :config
        (setq org-roam-directory pye/org-roam-directory)
        (org-roam-db-autosync-mode)

        ;; for org-roam-buffer-toggle
        ;; Recommendation in the official manual
        (add-to-list 'display-buffer-alist
          '("\\*org-roam\\*"
             (display-buffer-in-direction)
             (direction . right)
             (window-width . 0.33)
             (window-height . fit-window-to-buffer))))))
