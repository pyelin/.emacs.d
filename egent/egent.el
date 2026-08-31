;;; egent.el --- Session heads-up display for agent-shell  -*- lexical-binding: t -*-

;; Copyright (C) 2026

;; Author: Sreenivas Venkobarao
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (agent-shell "0.66.1") (acp "0.13.1") (posframe "1.4"))
;; Keywords: tools, convenience

;;; Commentary:

;; egent manages `agent-shell' sessions across projects.  It shows the
;; sessions open in Emacs and the ones only the agent still remembers, and
;; resumes either from the same list.
;;
;; Entry points:
;;
;;   `egent-sidebar-toggle'  workspace with a project-grouped sidebar
;;   `egent-peek'            transient posframe switcher (live sessions)
;;   `egent-resume'          pick a past session in any project
;;   `egent-name-session'    name a session using an external CLI
;;   `egent-rename-session'  name a session by hand
;;   `egent-usage-string'    pi's footer-style usage stats for a shell
;;
;; `persp-mode' and `posframe' are optional: the sidebar falls back to
;; restoring the window configuration, and peek is only loaded when
;; posframe is installed.

;;; Code:

(require 'egent-core)
(require 'egent-session-name)
(require 'egent-session)
(require 'egent-sidebar)
(require 'egent-usage)

;; Peek is the only surface that hard-depends on posframe.  Loading egent
;; without it should still give you the sidebar.
(when (require 'posframe nil 'noerror)
  (require 'egent-peek))

(provide 'egent)
;;; egent.el ends here
