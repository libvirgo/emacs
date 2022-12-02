(prefer-coding-system 'utf-8)
(setq el-get-sources
	  '((:name go-fill-struct :type elpa
			   ))
	  )
(setq go-mode-require-package
	  (append
	   '(go-mode
		 go-fill-struct
		 go-impl)
	  (mapcar 'el-get-as-symbol (mapcar 'el-get-source-name el-get-sources))))
(el-get 'sync go-mode-require-package)

(use-package go-mode
  :bind (:map go-mode-map
			  ("C-c C-u" . go-remove-unused-imports)))

(provide 'prelude-lang-go)
