;;; -*- lexical-binding: t; -*-

(use-package go-fill-struct
  :straight t)

(use-package go-mode
  :straight t
  :hook ((before-save . gofmt-before-save)
         (go-mode . (lambda ()
                      (with-eval-after-load 'embark
                        (make-local-variable 'embark-identifier-map)
                        (setq embark-identifier-map (copy-tree embark-identifier-map))
                        (define-key embark-identifier-map "f" #'go-fill-struct)
                        (define-key embark-identifier-map "a" #'eglot-code-actions)
                        (define-key embark-identifier-map "R" #'eglot-rename)))))
  :config
  (add-hook 'go-mode-hook 'eglot-ensure)
  (setq gofmt-command "goimports"))

(provide 'init-golang)
