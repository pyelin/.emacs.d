;;; early-init.el --- Early initialization -*- lexical-binding: t -*-
;;; Commentary:
;;; run in terminal
;;; ln -s ~/dev/.emacs.d/early-init.el ~/.emacs.d

;;; Code:

;; Prevent package.el from loading before straight.el takes over
(setq package-enable-at-startup nil)

;;; early-init.el ends here
