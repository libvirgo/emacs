;;; -*- lexical-binding: t; -*-

(use-package ejc-sql
  :hook ((sql-mode . ejc-sql-mode)
		 (ejc-sql-connected . (lambda ()
								(ejc-set-column-width-limit 255)))
		 (ejc-sql-minor-mode . (lambda ()
								 (require 'ejc-company)
								 (setq-local completion-at-point-functions (list (cape-super-capf
																	   (cape-company-to-capf #'ejc-company-backend)
																	   #'cape-dabbrev
																	   ))))))
  :config
  (setq clomacs-httpd-default-port 8090) ; Use a port other than 8080.
  (setq ejc-temp-editor-file-path clytie-sql-dir)
  (setq ejc-conn-statistics-file (expand-file-name "connection-statistics.el" clytie-sql-dir))
  (let ((sql-config (expand-file-name "sql-config.el" clytie-sql-dir)))
	(if (file-exists-p sql-config)
		(load sql-config)))
  )

(provide 'init-sql)
