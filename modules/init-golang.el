;;; -*- lexical-binding: t; -*-

(use-package go-fill-struct)

(use-package go-mode
  :straight (:files ("go-mode.el" "go-guru.el"))
  :mode ("\\.go\\'" . go-mode)
  :hook ((before-save . (lambda () (when (eq major-mode 'go-mode)
									 (gofmt))))
		 (go-mode . lsp)
         (go-mode . (lambda ()
						 (with-eval-after-load 'embark
                           (make-local-variable 'embark-identifier-map)
                           (setq embark-identifier-map (copy-tree embark-identifier-map))
                           (keymap-set embark-identifier-map "f" #'go-fill-struct)
                           (keymap-set embark-identifier-map "a" #'lsp-code-actions-at-point)
						   (keymap-set embark-identifier-map "s" #'lsp-ui-doc-show)
                           (keymap-set embark-identifier-map "R" #'lsp-rename))))
		 (go-mode . (lambda()
						 (setq c-default-style "bsd"
							   c-indent-level 4
							   c-basic-offset 4))))
  :mode-hydra
  (go-mode
   (:title "Go Commands" :color blue)
   ("Doc"
    (("d" godoc-at-point "doc at point"))
	"Goto"
	(("gf" consult-flymake "flymake goto")
	 ("gi" go-goto-method-receiver "goto method receiver")
	 ("ga" go-goto-arguments "goto func arguments")
	 ("gr" go-goto-return-values "goto return values")
	 )
	""
	(("gn" go-goto-function-name "goto func name")
	 ("gu" go-goto-imports "goto imports")
	 ("gF" go-goto-function "goto function")
	 ("gd" go-goto-docstring "goto docstring"))
	"Guru"
	(("r" go-guru-implements "find impl or interface.")
	 ("c" go-guru-callers "find func callers")
	 ("e" go-guru-callees "find func callees"))
    "Imports"
    (("ia" go-import-add "add")
     ("ir" go-remove-unused-imports "cleanup"))))
  :config
  (setq gofmt-command "goimports"))

(provide 'init-golang)
