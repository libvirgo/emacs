;;; -*- lexical-binding: t; -*-

(use-package rustic
  :init
  (setq rustic-rustfmt-args "+nightly"
		rustic-lsp-server 'rust-analyzer)
  :hook ((before-save . (lambda ()
						  (when (eq major-mode 'rustic-mode)
							(rustic-format-file))
						  ))
		 (rustic-mode . (lambda ()
						  "Enable auto-saving in rustic-mode buffers."
						  (when buffer-file-name
							(setq-local compilation-ask-about-save nil)))))
  :config
  (add-hook 'rustic-mode-hook 'lsp)
  )

(provide 'init-rust)
