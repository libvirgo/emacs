;;; -*- lexical-binding: t; -*-

(use-package go-fill-struct)

(use-package go-mode
  :straight (:files ("go-mode.el" "go-guru.el"))
  :hook ((before-save . (lambda () (when (eq major-mode 'go-ts-mode)
									 (require 'go-mode)
									 (gofmt))))
		 (go-ts-mode . lsp)
         (go-ts-mode . (lambda ()
						 (with-eval-after-load 'embark
                           (make-local-variable 'embark-identifier-map)
                           (setq embark-identifier-map (copy-tree embark-identifier-map))
                           (keymap-set embark-identifier-map "f" #'go-fill-struct)
                           (keymap-set embark-identifier-map "a" #'lsp-code-actions-at-point)
						   (keymap-set embark-identifier-map "s" #'lsp-ui-doc-show)
                           (keymap-set embark-identifier-map "R" #'lsp-rename)))))
  :mode-hydra
  (go-ts-mode
   (:title "Go Commands" :color blue)
   ("Doc"
    (("d" godoc-at-point "doc at point"))
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
