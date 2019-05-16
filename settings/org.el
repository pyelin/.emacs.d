(use-package org
  :mode (("\\.org\\'" . org-mode)
         ("\\.org.txt\\'" . org-mode))
  :config
  (setq org-image-actual-width nil)
  (setq org-log-done t)
  (setq org-babel-python-command "python3")
  (define-key global-map "\C-c l" 'org-store-link)
  (define-key global-map "\C-c a" 'org-agenda)
  (add-hook 'org-mode-hook
            (lambda () (setq show-trailing-whitespace f)))
  (add-hook 'org-mode-hook 'flyspell-mode)
  (org-babel-do-load-languages 'org-babel-load-languages '((js . t)))
  (org-babel-do-load-languages 'org-babel-load-languages '((python . t)))
  (org-babel-do-load-languages 'org-babel-load-languages '((sql . t)))
  (org-babel-do-load-languages 'org-babel-load-languages '((ditaa . t)))
  (setq org-ditaa-jar-path "/usr/local/Cellar/ditaa/0.11.0/libexec/ditaa-0.11.0-standalone.jar"))
