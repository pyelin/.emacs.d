;;; package --- Emacs configurations
;;; Commentary:

;;; Code:

(defvar pye/invoke-color "#40e0d0")
(defun umbra () "Umbra." (propertize "u" 'face '(:foreground "#178ccb" :bold t)))
(defun exa () "Exa." (propertize "e" 'face '(:foreground "#f48024" :bold t)))
(defun ova () "Ova." (propertize "o" 'face '(:foreground "#90bd31" :bold t)))
(defun hyper () "Umbra." (propertize "h" 'face '(:foreground "#178ccb" :bold t)))
(defun tera () "Exa." (propertize "t" 'face '(:foreground "#f48024" :bold t)))
(defun nora () "Ova." (propertize "n" 'face '(:foreground "#90bd31" :bold t)))
(defun hydra-invoker-format (first name &optional is-last)
  "Format NAME spell using FIRST, SECOND."
  (format (if is-last "[%s] %s" "[%s] %-5s") (funcall first) name))

(use-package hydra
  :init
  (setq hydra-is-helpful t)
  (setq hydra-hint-display-type 'lv))

(defun smart-open-line ()
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(defun smart-open-line-above ()
  "Insert an empty line above the current line.
Position the cursor at it's beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

(defun pye/before-invoke ()
  (set-cursor-color pye/invoke-color)
  (setq-default cursor-type 'box)
  (set-face-background hl-line-face "#1c6861"))

(defun pye/after-invoke ()
  (set-cursor-color "white")
  (setq-default cursor-type '(bar . 3))
  (set-face-background hl-line-face "gray25"))

(defhydra hydra-umbra (:hint none
                       :pre pye/before-invoke
                       :post pye/after-invoke)
"
?o? ?e? ?u?                ?h? ?t? ?n?
"
  ("o" hydra-refactor/body (hydra-invoker-format 'ova "LSP") :exit 1)
  ("e" avy-goto-word-1 (hydra-invoker-format 'exa "AVY") :exit 1)
  ("u" swiper-isearch (hydra-invoker-format 'umbra "SWIPER") :exit 1)
  ("h" counsel-rg (hydra-invoker-format 'hyper "RG") :exit 1)
  ("t" projectile-find-file (hydra-invoker-format 'tera "PROJECTILE"))
  ("n" counsel-find-file (hydra-invoker-format 'nora "FILE" t)))

(defhydra hydra-exa (:hint none
                     :pre pye/before-invoke
                     :post pye/after-invoke
                     :exit 1)
"
?o? ?e? ?u?                ?h? ?t? ?n?
"
  ("o" hydra-rectangle/body (hydra-invoker-format 'ova "RECTANGLE"))
  ("e" er/expand-region (hydra-invoker-format 'exa "EXPAND"))
  ("u" counsel-yank-pop (hydra-invoker-format 'umbra "YANK"))
  ("h" ivy-yasnippet (hydra-invoker-format 'hyper "SNIPPET"))
  ("t" switch-to-buffer (hydra-invoker-format 'tera "SWITCH"))
  ("n" list-buffers (hydra-invoker-format 'nora "BUFFERS")))

(defhydra hydra-ova (:hint none
                     :pre pye/before-invoke
                     :post pye/after-invoke
                     :exit 1)
"
?o? ?e? ?u?
"
  ("o" blanket (hydra-invoker-format 'ova "BLANKET"))
  ("e" pye/ivy-scratch (hydra-invoker-format 'exa "SCRATCH"))
  ("u" kill-this-buffer (hydra-invoker-format 'umbra "CLOSE"))
  ("h" nil)
  ("t" nil)
  ("n" nil))

(defhydra hydra-rectangle (:body-pre (rectangle-mark-mode 1)
                           :color pink
                           :post (deactivate-mark))
  "
  ^_c_^     _d_elete    _s_tring
_h_   _n_   _o_k        _y_ank
  ^_t_^     _j_new-copy _r_eset
^^^^        _e_xchange  _u_ndo
^^^^        ^ ^         _p_aste
"
  ("n" rectangle-forward-char nil)
  ("h" rectangle-backward-char nil)
  ("t" rectangle-next-line nil)
  ("c" rectangle-previous-line nil)
  ("e" hydra-ex-point-mark nil)
  ("j" copy-rectangle-as-kill nil)
  ("d" delete-rectangle nil)
  ("r" (if (region-active-p)
         (deactivate-mark)
         (rectangle-mark-mode 1)) nil)
  ("y" yank-rectangle nil)
  ("u" undo nil)
  ("s" string-rectangle nil)
  ("p" kill-rectangle nil)
  ("o" nil nil))

(defhydra hydra-refactor (:hint none
                     :exit 1)
"
?o? ?e? ?u? [_i_] IMENU [_r_] RENAME
"
  ("o" dumb-jump-go (hydra-invoker-format 'ova "DEF"))
  ("e" lsp-ui-flycheck-list (hydra-invoker-format 'exa "ERROR"))
  ("u" lsp-ui-peek-find-references (hydra-invoker-format 'umbra "REF"))
  ("i" lsp-ui-imenu "IMENU")
  ("r" lsp-rename "RENAME"))


(defhydra hydra-string-inflection (global-map "C-c u")
  "String inflection"
  ("_" (apply-function-to-region 'string-inflection-underscore-function) "underscore")
  ("-" (apply-function-to-region 'string-inflection-kebab-case-function) "kebab")
  ("c" (apply-function-to-region 'string-inflection-lower-camelcase-function) "camel")
  ("C" (apply-function-to-region 'string-inflection-camelcase-function) "Camel")
  ("u" (upcase-region (region-beginning) (region-end)) "UPPER")
  ("l" (downcase-region (region-beginning) (region-end)) "lower"))

(global-set-key (kbd "<f9>") 'pye/kill-other-buffers)
(global-set-key (kbd "<f7>") 'yankpad-capture-snippet)
(global-set-key (kbd "<f8>") 'yankpad-reload)
(global-set-key (kbd "M-u") 'hydra-umbra/body)
(global-set-key (kbd "M-e") 'hydra-exa/body)
(global-set-key (kbd "M-o") 'hydra-ova/body)
(global-set-key (kbd "C-M-\\") 'indent-relative)
(global-set-key (kbd "M-<up>") 'move-region-up)
(global-set-key (kbd "M-<down>") 'move-region-down)
