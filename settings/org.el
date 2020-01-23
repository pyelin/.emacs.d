(use-package org
  :mode (("\\.org\\'" . org-mode)
         ("\\.org.txt\\'" . org-mode))
  :config
  (setq org-image-actual-width nil)
  (setq org-log-done t)
  (setq org-directory "~/Dropbox/Notes")
  (setq org-agenda-files '("~/Dropbox/Notes/"))
  (setq org-babel-python-command "python3")
  (define-key global-map "\C-cl" 'org-store-link)
  (define-key global-map "\C-ca" 'org-agenda)
  (add-hook 'org-mode-hook
    (lambda () (setq show-trailing-whitespace nil)))
  (add-hook 'org-mode-hook 'flyspell-mode)
  (org-babel-do-load-languages 'org-babel-load-languages '((js . t)))
  (org-babel-do-load-languages 'org-babel-load-languages '((python . t)))
  (org-babel-do-load-languages 'org-babel-load-languages '((sql . t)))
  (org-babel-do-load-languages 'org-babel-load-languages '((ditaa . t))))

(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package org-super-agenda
  :ensure t
  :config
  (setq org-super-agenda-groups
    '((:name "Today"
				:time-grid t
				:scheduled today)
       (:name "Due today"
         :deadline today)
       (:name "Important"
         :priority "A")
       (:name "Overdue"
         :deadline past)
       (:name "Due soon"
         :deadline future)
       (:name "Waiting"
         :todo "WAIT"))))
