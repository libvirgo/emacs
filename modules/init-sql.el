(use-package ejc-sql
  :config
  (setq clomacs-httpd-default-port 8090) ; Use a port other than 8080.
  (setq ejc-temp-editor-file-path clytie-sql-dir)
  (setq ejc-conn-statistics-file (expand-file-name "connection-statistics.el" clytie-sql-dir))
  (let ((sql-config (expand-file-name "sql-config.el" clytie-sql-dir)))
	(if (file-exists-p sql-config)
		(load sql-config)))
  )

(provide 'init-sql)
