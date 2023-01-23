;;; -*- lexical-binding: t; -*-

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))) ;; Configure orderless
  (with-eval-after-load 'embark
    (push 'embark--allow-edit
          (alist-get 'lsp-rename embark-target-injection-hooks)))
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
  :hook
  ((lsp-mode . (lambda () (yas-minor-mode-on)))
   (lsp-completion-mode . my/lsp-mode-setup-completion))
  :custom
  (lsp-completion-provider :none) ;; we use Corfu!
  (lsp-modeline-code-actions-enable nil)
  (lsp-modeline-workspace-status-enable nil)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-lens-enable nil)
  (lsp-eldoc-enable-hover t)
  ;; (lsp-signature-auto-activate nil)
  (lsp-signature-doc-lines 0)
  (lsp-session-file (expand-file-name "lsp-session-v1" clytie-cache-dir))
  )

(use-package lsp-ui
  :hook
  ((lsp-ui-mode . (lambda ()
					(with-eval-after-load 'embark
					  (make-local-variable 'embark-identifier-map)
					  (setq embark-identifier-map (copy-tree embark-identifier-map))
					  (keymap-set embark-identifier-map "M-." #'lsp-ui-peek-find-definitions)
					  (keymap-set embark-identifier-map "M-?" #'lsp-ui-peek-find-references)))))
  :custom
  (lsp-ui-sideline-enable nil)
  (lsp-ui-doc-show-with-cursor nil)
  (lsp-ui-doc-include-signature t)
  ;; (lsp-ui-doc-use-webkit t)
  (lsp-ui-doc-position 'at-point))

(provide 'init-lsp)
