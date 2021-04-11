(cl-defmethod eglot-execute-command
  ((server eglot-sqls) (command (eql executeQuery)) arguments)
  "sqls: For executeQuery."
  ;; (ignore-errors
  (let*
    (
      (beg (eglot--pos-to-lsp-position (if (use-region-p) (region-beginning) (point-min))))
      (end (eglot--pos-to-lsp-position (if (use-region-p) (region-end) (point-max))))
      (res
        (jsonrpc-request server :workspace/executeCommand
          `(
             :command ,(format "%s" command)
             :arguments ,arguments
             :timeout 0.5
             :range (:start ,beg :end ,end))))
      (buffer (generate-new-buffer "*sqls*")))
    (with-current-buffer buffer
      (eglot--apply-text-edits
        `[
           (:range
             (:start
               (:line 0 :character 0)
               :end
               (:line 0 :character 0))
             :newText ,res
             )
           ]
        )
      (toggle-truncate-lines))
    (pop-to-buffer buffer)))

(cl-defmethod eglot-execute-command
  ((server eglot-sqls) (_cmd (eql switchDatabase)) arguments)
  "sqls; For switchDatabase."
  (let*
    (
      (res (jsonrpc-request server :workspace/executeCommand
             `(:command "showDatabases" :arguments ,arguments :timeout 0.5)))
      (menu-items (split-string res "\n"))
      (menu `("Eglot code actions:" ("dummy" ,@menu-items)))
      (db
        (if (listp last-nonmenu-event)
          (x-popup-menu last-nonmenu-event menu)
          (completing-read "[eglot] Pick an database: "
            menu-items nil t
            nil nil (car menu-items)))))
    (jsonrpc-request server :workspace/executeCommand
      `(:command "switchDatabase" :arguments [,db] :timeout 0.5))))

(use-package eglot
  :ensure t
  :bind
  (:map eglot-mode-map
	  ("C-c C-d" . completion-at-point)
	  ("C-c C-r" . eglot-code-actions)
  )

  :hook ((sql-mode . eglot-ensure)
  )

  :config
  (add-hook 'eglot--managed-mode-hook (lambda () (flymake-mode -1)))
  ;; [install-dependency]
  ;; npm i -g typescript typescript-language-server
  (add-hook 'js2-mode 'eglot-ensure)
  ;; pip install python-language-server
  (add-hook 'python-mode 'eglot-ensure)
  ;; ghcup install hls
  (add-hook 'haskell-mode 'eglot-ensure)
  ;; https://github.com/lighttiger2505/sqls
  (add-hook 'sql-mode 'eglot-ensure)

  ;; sqls
  (setq eglot-sync-connect 1)
  (add-to-list
    'display-buffer-alist
    '("\\*sqls\\*"
       (display-buffer-reuse-window display-buffer-at-bottom)
       (reusable-frames . visible)
       (window-height . 0.3)))

  (defclass eglot-sqls (eglot-lsp-server) () :documentation "SQL's Language Server")
  (add-to-list 'eglot-server-programs '(sql-mode . (eglot-sqls "sqls"))))
