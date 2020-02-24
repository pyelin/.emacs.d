(use-package org
  :mode (("\\.org\\'" . org-mode)
         ("\\.org.txt\\'" . org-mode))
  :config
  (setq org-image-actual-width nil)
  (setq org-log-done t)
  (setq org-todo-keywords '((sequence "TODO" "REVIEW" "ACCEPTED" "DONE")))
  (setq org-todo-keyword-faces
    '(("REVIEW" . (:foreground "hot pink"))
      ("ACCEPTED" . (:foreground "cyan"))))
  (setq org-directory "~/Dropbox/Notes")
  (setq org-agenda-files '("~/Dropbox/Notes/"))
  (setq org-confirm-babel-evaluate nil)
  (setq org-babel-python-command "python3")
  (add-to-list 'org-structure-template-alist '("n" "#+NAME: ?"))
  (define-key global-map "\C-cl" 'org-store-link)
  (define-key global-map "\C-ca" 'org-agenda)
  (define-key org-mode-map (kbd "M-e") nil) ;; reserved for keybinding
  (add-hook 'org-mode-hook
    (lambda () (setq show-trailing-whitespace nil)))
  (add-hook 'org-mode-hook 'flyspell-mode)
  (add-hook 'org-mode-hook 'yas-minor-mode)
  (org-babel-do-load-languages 'org-babel-load-languages '((js . t)))
  (org-babel-do-load-languages 'org-babel-load-languages '((python . t)))
  (org-babel-do-load-languages 'org-babel-load-languages '((sql . t)))
  (org-babel-do-load-languages 'org-babel-load-languages '((shell . t))))

(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package org-super-agenda
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

(use-package org-gcal
  :config
  (setq
    org-gcal-client-id pye-env/picnic-calendar-client-id
    org-gcal-client-secret pye-env/picnic-calendar-client-secret
    org-gcal-file-alist '(("pye@picnichealth.com" .  "~/Dropbox/Notes/picnic_calendar.org")))
  (cond
    (pye-env/picnic-calendar-autofetch
      (progn
        (add-hook 'org-agenda-mode-hook 'org-gcal-fetch)
        (add-hook 'org-agenda-finalize-hook 'org-gcal-fetch)))))

(use-package ob-restclient
  :config
  (org-babel-do-load-languages
    'org-babel-load-languages
    '((restclient . t))))
