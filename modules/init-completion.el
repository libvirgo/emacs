;;; -*- lexical-binding: t; -*-

(use-package kind-icon
  :straight t
  :defer 2
  :custom
  (kind-icon-use-icons nil)
  (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
  )

(use-package corfu
  :straight (corfu :type git :host github :repo "minad/corfu" :files ("corfu.el" "extensions/corfu-history.el"))
  :bind ((:map corfu-map ("S-SPC" . corfu-insert-separator)))
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-separator ?\s)          ;; Orderless field separator
  (corfu-auto-delay 0)
  (corfu-quit-no-match 't)
  (savehist-mode)
  (add-to-list 'savehist-additional-variables 'corfu-history)
  :init
  (global-corfu-mode)
  (corfu-history-mode))

(use-package cape
  :straight t
  :bind (
         ("C-c c p" . completion-at-point))
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  )

(use-package dabbrev
  :straight t
  :bind (("C-c c d" . dabbrev-completion)))

(use-package orderless
  :straight t
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;; orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '(
                                        (file (styles partial-completion))
                                        (eglot (styles orderless)))))

(use-package eglot
  :straight t
  :bind (:map eglot-mode-map
              ("C-c l a" . eglot-code-actions)
              ("C-c l r" . eglot-rename)
              ))


(use-package corfu-english-helper
  :straight (corfu-english-helper :type git :host github :repo "manateelazycat/corfu-english-helper" :feature orfu-english-helper-data)
  :bind (
         ("C-h C-e" . corfu-english-helper-search)))

(use-package citre
  :straight t
  :bind (
         ("C-c e d" . citre-peek)
         ("C-c e r" . citre-peek-reference)
         ("C-c e b" . citre-jump-back)
         ("C-c e g" . citre-peek-restore)
         :map citre-peek-keymap
         ("C-j" . citre-peek-jump)
         ("C-t" . citre-peek-through)
         ("C-u" . citre-peek-through-reference)
         ("C-n" . citre-peek-next-line)
         ("C-p" . citre-peek-prev-line)
         ("M-n" . citre-peek-next-tag)
         ("M-p" . citre-peek-prev-tag)
         ("C-f" . citre-peek-chain-forward)
         ("C-b" . citre-peek-chain-backward)
         )
  )

(provide 'init-completion)
