(use-package consult
  :bind
  ([remap goto-line] . consult-goto-line)
  ([remap isearch-forward] . consult-line)
  ([remap switch-to-buffer] . consult-buffer)
  ("C-h M" . consult-minor-mode-menu)
  :custom
  (consult-line-start-from-top t)
  (xref-show-definitions-function #'consult-xref)
  (xref-show-xrefs-function #'consult-xref)
  :hook
  (org-mode . (lambda () (setq-local consult-fontify-preserve nil))))


(use-package corfu
  :hook
  (after-init . global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-delay .5))

(use-package corfu-terminal
  :straight '(corfu-terminal
     :type git
     :repo "https://codeberg.org/akib/emacs-corfu-terminal.git")
  :config
  (unless (display-graphic-p)
    (corfu-terminal-mode +1)))

(use-package marginalia
  :hook
  (after-init . marginalia-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless))
  (orderless-component-separator 'orderless-escapable-split-on-space)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package vertico
  :custom
  (vertico-count-format '("%-5s " . "%2$s"))
  (vertico-resize nil)
  :hook
  (after-init . vertico-mode))

(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                  (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode)
  (embark-collect-mode . (lambda () (setq show-trailing-whitespace nil))))
