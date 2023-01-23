;;; -*- lexical-binding: t; -*-

(use-package go-fill-struct)

(use-package go-mode
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
  :config
  (setq gofmt-command "goimports"))

(provide 'init-golang)
