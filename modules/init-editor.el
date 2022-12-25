;;; -*- lexical-binding: t; -*-

(use-package awesome-pair
  :straight (awesome-pair :type git :host github :repo "manateelazycat/awesome-pair")
  :hook
  (lisp-mode)
  (emacs-lisp-mode)
  (lisp-interaction-mode)
  (go-mode)
  :bind (:map awesome-pair-mode-map
              ("(" . awesome-pair-open-round)
              ("[" . awesome-pair-open-bracket)
              ("{" . awesome-pair-open-curly)
              (")" . awesome-pair-close-round)
              ("]" . awesome-pair-close-bracket)
              ("}" . awesome-pair-close-curly)
              ("=" . awesome-pair-equal)
              ))

(use-package hl-indent-scope
  :straight t
  :commands (hl-indent-scope-mode)
  :hook(
        (prog-mode . hl-indent-scope-mode)
        (after-load-theme . hl-indent-scope--auto-color-calc)))

(use-package rime
  :straight (rime :type git
                  :host github
                  :repo "DogLooksGood/emacs-rime"
                  :files ("*.el" "Makefile" "lib.c"))
  :custom
  (default-input-method "rime")
  (rime-emacs-module-header-root "/opt/homebrew/Cellar/emacs-plus@29/29.0.60/include")
  (rime-librime-root (expand-file-name "librime/dist" prelude-local-dir))
  (rime-show-candidate 'posframe)
  (rime-user-data-dir "~/Library/Rime")
  (rime-posframe-style 'vertical)
  :bind (:map rime-mode-map ("M-j" . rime-force-enable))
  :config
  (setq rime-disable-predicates '(meow-normal-mode-p
				  meow-motion-mode-p
				  meow-keypad-mode-p
				  rime-predicate-prog-in-code-p
				  rime-predicate-after-alphabet-char-p))
  (setq rime-inline-predicates '(rime-predicate-space-after-cc-p
				                 rime-predicate-current-uppercase-letter-p)))

(provide 'init-editor)
