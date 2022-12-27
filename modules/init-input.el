;;; -*- lexical-binding: t; -*-

(use-package rime
  :custom
  (default-input-method "rime")
  (rime-emacs-module-header-root "/opt/homebrew/Cellar/emacs-plus@29/29.0.60/include")
  (rime-librime-root (expand-file-name "librime/dist" clytie-lib-dir))
  (rime-show-candidate 'posframe)
  (rime-user-data-dir "~/Library/Rime")
  (rime-posframe-style 'vertical)
  :bind (:map rime-mode-map ("M-j" . rime-inline-ascii))
  :config
  (setq rime-disable-predicates '(meow-normal-mode-p
				  meow-motion-mode-p
				  meow-keypad-mode-p
				  rime-predicate-prog-in-code-p
				  rime-predicate-after-alphabet-char-p))
  (setq rime-inline-predicates '(rime-predicate-space-after-cc-p
				 rime-predicate-current-uppercase-letter-p)))

(provide 'init-input)
