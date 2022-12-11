;;; -*- lexical-binding: t; -*-

(use-package go-mode
  :straight t
  :hook ((before-save . gofmt-before-save))
  :bind (:map go-mode-map
			  ("C-c C-u" . go-remove-unused-imports)
              ("C-c C-l C-f" . go-fill-struct))
  :config
  (add-hook 'go-mode-hook 'eglot-ensure)
  (setq gofmt-command "goimports"))

(provide 'init-golang)
