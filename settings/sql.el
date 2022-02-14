(use-package sqlformat
  :commands (sqlformat sqlformat-buffer sqlformat-region)
  :hook (sql-mode . sqlformat-on-save-mode)
  :init
  (setq sqlformat-command 'pgformatter
    sqlformat-args
    '("-s2"
       "-w80"
       "-g"
       "--comma-start"
       "--format-type"
       "--keep-newline")))
