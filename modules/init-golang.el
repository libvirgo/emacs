;;; -*- lexical-binding: t; -*-

(use-package go-ts-mode
  :hook ((before-save . gofmt-before-save)
         (go-ts-mode . (lambda ()
                      (with-eval-after-load 'embark
                        (make-local-variable 'embark-identifier-map)
                        (setq embark-identifier-map (copy-tree embark-identifier-map))
                        (define-key embark-identifier-map "f" #'go-fill-struct)
                        (define-key embark-identifier-map "a" #'eglot-code-actions)
                        (define-key embark-identifier-map "R" #'eglot-rename)))))
  :init
  (define-derived-mode go-mode go-ts-mode "Go")
  :config
  (add-hook 'go-ts-mode-hook 'eglot-ensure)
  (setq gofmt-command "goimports"))

(provide 'init-golang)
