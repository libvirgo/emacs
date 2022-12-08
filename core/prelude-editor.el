(use-package hungry-delete
  :straight t
  :init (global-hungry-delete-mode)
  )

(use-package smartparens
  :straight t)

(use-package avy
  :straight t
  :bind(("C-c g l" . avy-goto-line)
        ("C-c g c" . avy-goto-char)
        ("C-c g w" . avy-goto-char-2)))

(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

(provide 'prelude-editor)
