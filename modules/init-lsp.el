;;; -*- lexical-binding: t; -*-

(use-package lsp-mode
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
  :hook
  ((lsp-mode . (lambda () (yas-minor-mode-on))))
  :init
  (setq lsp-keymap-prefix "C-c l")
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))) ;; Configure orderless
  :hook
  (lsp-completion-mode . my/lsp-mode-setup-completion))

(use-package lsp-ui
  :custom
  (lsp-ui-sideline-enable nil)
  (lsp-ui-doc-show-with-cursor nil)
  (lsp-ui-doc-include-signature t)
  ;; (lsp-ui-doc-use-webkit t)
  (lsp-ui-doc-position 'at-point)
  )

(provide 'init-lsp)
