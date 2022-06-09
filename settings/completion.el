(use-package consult
  :bind
  ([remap goto-line] . consult-goto-line)
  ([remap isearch-forward] . consult-line)
  ([remap switch-to-buffer] . consult-buffer)
  ("C-h M" . consult-minor-mode-menu)
  :custom
  (consult-line-start-from-top t)
  (consult-project-root-function #'me/project-root)
  (xref-show-definitions-function #'consult-xref)
  (xref-show-xrefs-function #'consult-xref)
  :hook
  (org-mode . (lambda () (setq-local consult-fontify-preserve nil)))
  :init
  (with-eval-after-load 'evil
    (evil-global-set-key 'motion "gm" #'consult-mark)
    (evil-global-set-key 'motion "gM" #'consult-imenu)
    (evil-global-set-key 'motion "go" #'consult-outline)))


(use-package corfu
  :hook
  (after-init . global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-delay .5))

(use-package marginalia
  :hook
  (after-init . marginalia-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless))
  (orderless-component-separator 'orderless-escapable-split-on-space))

(use-package vertico
  :custom
  (vertico-count-format '("%-5s " . "%2$s"))
  (vertico-resize nil)
  :hook
  (after-init . vertico-mode))
