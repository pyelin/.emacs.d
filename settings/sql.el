(use-package sqlformat
  :config
  (setq sqlformat-command 'pgformatter)
  ;; -s2 for 2-space indent
  (setq sqlformat-args '("-s2")))
