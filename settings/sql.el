(use-package sqlformat
  :commands (sqlformat sqlformat-buffer sqlformat-region)
  :init
  (setq sqlformat-command 'sqlfluff
    sqlformat-args '("--dialect" "duckdb")))

(use-package csv-mode)
