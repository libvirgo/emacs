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

(provide 'init-editor)
