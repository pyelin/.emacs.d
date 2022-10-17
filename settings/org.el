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

(use-package org
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
  ;; (add-to-list 'org-structure-template-alist '("n" "#+NAME: ?"))
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

;; Set up hydra for org template
;; https://github.com/abo-abo/hydra/wiki/Org-mode-block-templates
(defhydra hydra-org-template (:color blue :hint nil)
  "
 _e_macs-lisp    s_q_l      _s_rc      _p_ython       _j_avascript:
 _E_xample
"
  ("e" (hot-expand "<s" "emacs-lisp"))
  ("E" (hot-expand "<e"))
  ("q" (hot-expand "<s" "sql"))
  ("s" (hot-expand "<s"))
  ("p" (hot-expand "<s" "python"))
  ("j" (hot-expand "<s" "javascript"))
  ("<" self-insert-command "ins")
  ("o" nil "quit"))

;; Reset the org-template expension system, this is need after upgrading to org 9 for some reason
(setq org-structure-template-alist (eval (car (get 'org-structure-template-alist 'standard-value))))

(defun hot-expand (str &optional mod header)
  "Expand org template.

STR is a structure template string recognised by org like <s. MOD is a
string with additional parameters to add the begin line of the
structure element. HEADER string includes more parameters that are
prepended to the element after the #+HEADER: tag."
  (let (text)
    (when (region-active-p)
      (setq text (buffer-substring (region-beginning) (region-end)))
      (delete-region (region-beginning) (region-end))
      (deactivate-mark))
    (when header (insert "#+HEADER: " header) (forward-line))
    (insert str)
    (org-tempo-complete-tag)
    (when mod (insert mod) (forward-line))
    (when text (insert text))))

(define-key org-mode-map "<"
  (lambda () (interactive)
    (if (or (region-active-p) (looking-back "^"))
      (hydra-org-template/body)
      (self-insert-command 1))))

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
