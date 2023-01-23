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
   (:title "Go Commands")
   ("Doc"
    (("d" godoc-at-point "doc at point" :color blue))
	"Guru"
	(("r" go-guru-implements "find impl or interface." :color blue)
	 ("c" go-guru-callers "find func callers" :color blue)
	 ("e" go-guru-callees "find func callees" :color blue))
    "Imports"
    (("ia" go-import-add "add" :color blue)
     ("ir" go-remove-unused-imports "cleanup" :color blue))))
  :config
  (setq gofmt-command "goimports"))

(provide 'init-golang)
