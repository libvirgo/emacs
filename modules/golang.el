(add-recipe-items
 '((:name go-fill-struct :type elpa))
 )
(setq go-mode-require-package
	  (append
	   '(go-mode
		 go-fill-struct
		 go-impl)
	  (mapcar 'el-get-as-symbol (mapcar 'el-get-source-name el-get-sources))))
(el-get 'sync go-mode-require-package)

(use-package go-mode
  :hook ((before-save . gofmt-before-save))
  :bind (:map go-mode-map
			  ("C-c C-u" . go-remove-unused-imports))
  :config
  (add-hook 'go-mode-hook 'eglot-ensure)
  (setq gofmt-command "goimports"))

(provide 'golang)
