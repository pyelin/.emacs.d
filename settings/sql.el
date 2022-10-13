(use-package sqlformat
  :commands (sqlformat sqlformat-buffer sqlformat-region)
  :init
  (setq sqlformat-command 'pgformatter
    sqlformat-args
    '("-s2"
       "-w90"
       "--nogrouping"
       "--comma-break"
       "--format-type"
       "--keep-newline")))

(use-package csv-mode)
