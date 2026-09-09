;;; python.el --- Python -*- lexical-binding: t; -*-
;;; Commentary:
;; One language server for Python: `pylsp', with the `python-lsp-ruff' plugin
;; supplying lint + format.  Eglot only ever attaches a single server per
;; (project, major-mode), so running ruff standalone means no goto-definition
;; ("Unsupported or ignored LSP capability :definitionProvider") -- ruff's
;; server is lint/format only.  pylsp gives jedi-backed xref/completion/hover
;; and hosts ruff as a plugin, so both work from one process.
;;
;; Install (outside Emacs):
;;   uv tool install python-lsp-server --with python-lsp-ruff
;;; Code:

(require 'python)
(require 'treesit)
(require 'eglot)

(setq python-shell-interpreter "python3")

;;;; Major mode ----------------------------------------------------------

;; `treesit-enabled-modes' (init.el) already remaps python-mode when the
;; grammar is present; make it explicit so mode selection doesn't depend on
;; load order, and so the remap is skipped rather than erroring if the
;; grammar is ever missing (\\[treesit-install-language-grammar] python).
(if (treesit-ready-p 'python 'quiet)
    (progn
      (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
      (add-to-list 'auto-mode-alist '("\\.pyi\\'" . python-ts-mode)))
  (message "python: tree-sitter grammar missing, falling back to python-mode"))

;;;; Virtualenv discovery -------------------------------------------------

(defun pye/python-venv-root ()
  "Return the virtualenv directory to hand to jedi, or nil.
Prefers an activated VIRTUAL_ENV, else the nearest `.venv' above the
current file."
  (or (getenv "VIRTUAL_ENV")
      (let ((dir (locate-dominating-file
                  (or buffer-file-name default-directory) ".venv")))
        (when dir (expand-file-name ".venv" dir)))))

;;;; Server selection -----------------------------------------------------

;; Eglot's built-in Python entry is an `eglot-alternatives' list ending in
;; ("ruff" "server") / "ruff-lsp".  With only ruff on PATH it silently picks
;; that.  Prepending our own entry pins pylsp instead.
(add-to-list 'eglot-server-programs
             '((python-mode python-ts-mode) . ("pylsp")))

;;;; Workspace configuration ----------------------------------------------

(defun pye/pylsp-workspace-configuration (_server)
  "Return pylsp settings: ruff for lint/format, jedi for navigation.

Eglot evaluates this in a temp buffer rooted at the project directory with
`major-mode' set to the server's first managed mode -- it never looks at the
visiting buffer -- so the value must be global and self-guarding."
  (when (provided-mode-derived-p major-mode 'python-base-mode)
    `(:pylsp
      (:plugins
       ( :ruff ( :enabled t
                 :formatEnabled t
                 :lineLength 88
                 ;; Fix these on format; everything else is reported only.
                 :format ["I"]        ; sort imports
                 :unsafeFixes :json-false)

         ;; Everything pylsp ships that duplicates or fights ruff.
         :pycodestyle (:enabled :json-false)
         :pyflakes    (:enabled :json-false)
         :mccabe      (:enabled :json-false)
         :flake8      (:enabled :json-false)
         :pylint      (:enabled :json-false)
         :autopep8    (:enabled :json-false)
         :yapf        (:enabled :json-false)

         ;; Rope is slow on big trees and jedi already covers these.
         :rope_autoimport (:enabled :json-false)
         :rope_completion (:enabled :json-false)

         :jedi_completion ( :enabled t :include_params t :fuzzy t
                            :eager :json-false)
         :jedi_definition ( :enabled t
                            :follow_imports t
                            :follow_builtin_imports t)
         :jedi_hover          (:enabled t)
         :jedi_references     (:enabled t)
         :jedi_signature_help (:enabled t)
         :jedi_symbols        (:enabled t :all_scopes :json-false)
         ,@(when-let* ((venv (pye/python-venv-root)))
             `(:jedi (:environment ,venv))))))))

;; Global, not buffer-local: `eglot--workspace-configuration-plist' reads this
;; from a temp buffer, where only dir-locals apply.  Dir-locals still win,
;; since they set a local value in that temp buffer.
(setq-default eglot-workspace-configuration
              #'pye/pylsp-workspace-configuration)

;;;; Buffer setup ---------------------------------------------------------

;; Ruff's diagnostics reach the buffer through `eglot-flymake-backend', which
;; `init.el' keeps out of every *other* eglot-managed mode.
(add-hook 'python-base-mode-hook #'eglot-ensure)

;;;; Format on save -------------------------------------------------------

(defun pye/python--format-on-save ()
  "Run `ruff format' (via pylsp) on save, when a server is attached."
  (when (eglot-managed-p)
    (eglot-format-buffer)))

(defun pye/python--enable-format-on-save ()
  (add-hook 'before-save-hook #'pye/python--format-on-save nil t))

(add-hook 'python-base-mode-hook #'pye/python--enable-format-on-save)

;;; python.el ends here
