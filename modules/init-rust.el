;;; -*- lexical-binding: t; -*-

(use-package rustic
  :hook ((before-save . (lambda ()
						  (when (eq major-mode 'rustic-mode)
							(rustic-format-file))
						  ))
		 (rustic-mode . (lambda ()
						  "Enable auto-saving in rustic-mode buffers."
						  (when buffer-file-name
							(setq-local compilation-ask-about-save nil)))))
  :init
  (setq rustic-rustfmt-args "+nightly"
		rustic-lsp-server 'rust-analyzer
		rustic-lsp-client 'eglot)
  :config
  (add-hook 'rustic-mode-hook ' eglot-ensure)
  )

(provide 'init-rust)
