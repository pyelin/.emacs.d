;;; package --- Emacs configurations
;;; Commentary:

;;; Code:


(defun umbra () "Umbra." (propertize "u" 'face '(:foreground "sky blue" :bold t)))
(defun electra () "Electra." (propertize "e" 'face '(:foreground "#C0362C" :bold t)))
(defun ova () "Ova." (propertize "o" 'face '(:foreground "#ddbc91" :bold t)))
(defun hydra-invoker-format (first second third name &optional is-last)
  "Format NAME spell using FIRST, SECOND, THIRD."
  (format (if is-last "%s%s%s %s" "%s%s%s %-8s") (funcall first) (funcall second) (funcall third) name))

(use-package hydra
  :init
  (setq hydra-is-helpful t))

(defhydra hydra-invoker (:pre (progn
                                (set-cursor-color "#40e0d0")
                                (setq-default cursor-type 'box))
                          :post (progn
                                  (set-cursor-color "#C0362C")
                                  (setq-default cursor-type '(bar . 2)))
                          :hint nil
                          (global-map "M-SPC"))
  "
?u u u? ?u e e? ?e e e? ?o o o? ?o e e?
?u u e? ?u e u? ?e e u? ?o o u? ?o e u?
?u u o? ?u e o? ?e e o? ?o o e? ?o e o?
"
  ;; char
  ("n" forward-char)
  ("h" backward-char)
  ("t" next-line)
  ("c" previous-line)
  ("g" delete-backward-char)
  ("r" delete-char)
  ;; word
  ("M-n" forward-word)
  ("M-h" backward-word)
  ("M-g" backward-kill-word)
  ("M-r" kill-word)
  ;; forward
  ("s" end-of-line)
  ("d" beginning-of-line)
  ;; mark
  ("m" set-mark-command)
  ("q" kill-region)
  ("j" kill-ring-save)
  (";" undo)
  ("k" yank)
  ;; window
  ("1" delete-other-windows :exit 1)
  ("2" (progn (split-window-below) (other-window 1)))
  ("3" (progn (split-window-right) (other-window 1)))
  ("4" delete-window)
  ;; sexp
  ("7" er/mark-inside-pairs)
  ("8" er/expand-region)
  ("*" er/contract-region)
  ;; invoke
  ;; umbra
  ("u u u" save-buffer (hydra-invoker-format 'umbra 'umbra 'umbra "save"))
	("u u e" avy-goto-char-2 (hydra-invoker-format 'umbra 'umbra 'electra "avy"))
  ("u u o" swiper (hydra-invoker-format 'umbra 'umbra 'ova "swiper") :exit 1)
 	("u e e" ivy-yasnippet (hydra-invoker-format 'umbra 'electra 'electra "yas") :exit 1)
	("u e u" counsel-yank-pop (hydra-invoker-format 'umbra 'electra 'umbra "yank"))
  ("u e o" counsel-yank-pop (hydra-invoker-format 'umbra 'electra 'ova "yank"))
	("u o u" nil)
	("u o e" nil)
  ("u o o" nil)
  ;; electra
	("e e e" ag (hydra-invoker-format 'electra 'electra 'electra "ag") :exit t)
	("e e u" query-replace (hydra-invoker-format 'electra 'electra 'umbra "replace") :exit t)
	("e e o" hydra-rectangle/body (hydra-invoker-format 'electra 'electra 'ova "rect") :exit t)
	("e u e" nil)
	("e u u" nil)
	("e u o" nil)
	("e o e" nil)
	("e o u" nil)
  ("e o o" nil)
  ;; omega
  ("o o o" switch-to-buffer (hydra-invoker-format 'ova 'ova 'ova "buffer") :exit 1)
  ("o o u" next-buffer (hydra-invoker-format 'ova 'ova 'umbra "prev"))
  ("o o e" previous-buffer (hydra-invoker-format 'ova 'ova 'electra "next"))
  ("o u e" nil)
 	("o u u" nil)
	("o u o" nil)
  ("o e e" dired-jump (hydra-invoker-format 'ova 'electra 'electra "dired" t))
  ("o e u" counsel-find-file (hydra-invoker-format 'ova 'electra 'umbra "file" t) :exit t)
  ("o e o" counsel-projectile-find-file (hydra-invoker-format 'ova 'electra 'ova "proj" t) :exit t)
  ("SPC" nil))

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

(defhydra hydra-string-inflection (global-map "C-c u")
  "String inflection"
  ("_" (apply-function-to-region 'string-inflection-underscore-function) "underscore")
  ("-" (apply-function-to-region 'string-inflection-kebab-case-function) "kebab")
  ("u" (apply-function-to-region 'string-inflection-upcase-function) "uppercase")
  ("c" (apply-function-to-region 'string-inflection-lower-camelcase-function) "camel")
  ("C" (apply-function-to-region 'string-inflection-camelcase-function) "Camel"))

(global-set-key (kbd "M-SPC") 'hydra-invoker/body)
