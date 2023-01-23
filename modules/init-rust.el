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
  :mode-hydra
  (rustic-mode
   (:title "Rust Commands" :color blue)
   ("Dependencies"
	(("da" rustic-cargo-add "cargo add")
	 ("dr" rustic-cargo-rm "cargo rm"))
	"Run"
	(("rr" rustic-cargo-run "cargo run")
	 ("rt" rustic-cargo-test "cargo test")
	 ("rb" rustic-cargo-bench "cargo bench"))
	"Utils"
	(("uf" rustic-cargo-fmt "cargo fmt")
	 ("ud" rustic-cargo-doc "cargo doc")
	 ("ub" rustic-cargo-build "cargo build")
	 ("uc" rustic-cargo-check "cargo check"))
	))
  :config
  (add-hook 'rustic-mode-hook 'lsp)
  )

(provide 'init-rust)
