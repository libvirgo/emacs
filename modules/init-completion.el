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
  :straight (corfu :type git :host github :repo "minad/corfu" :files ("corfu.el" "extensions/corfu-quick.el"))
  :bind ((:map corfu-map
               ("C-SPC" . corfu-insert-separator)
               ("C-n" . corfu-complete-common-or-next)
               ("C-f" . corfu-quick-complete)))
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-separator ?\s)          ;; Orderless field separator
  (corfu-auto-delay 0)
  (corfu-quit-no-match 't)
  :init
  (defun corfu-complete-common-or-next ()
    "Complete common prefix or go to next candidate."
    (interactive)
    (if (= corfu--total 1)
        (progn
          (corfu--goto 1)
          (corfu-insert))
      (let* ((input (car corfu--input))
             (str (if (thing-at-point 'filename) (file-name-nondirectory input) input))
             (pt (length str))
             (common (try-completion str corfu--candidates)))
        (if (and (> pt 0)
                 (stringp common)
                 (not (string= str common)))
            (insert (substring common pt))
          (corfu-next)))))
  (global-corfu-mode))

(use-package cape
  :straight t
  :bind (
         ("C-c c p" . completion-at-point)
         ("C-c c d" . cape-dabbrev)
         ("C-c c f" . cape-file))
  :hook
  ((emacs-lisp-mode . (lambda ()
                        (setq-local completion-at-point-functions
                                    (list (cape-super-capf
                                           #'elisp-completion-at-point
                                           #'cape-dabbrev))))))
  :init
  (setq cape-dabbrev-min-length 1)
  (setq cape-dabbrev-check-other-buffers t)
  ;; (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  )

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
  :init
  (with-eval-after-load 'embark
      (push 'embark--allow-edit
            (alist-get 'eglot-rename embark-target-injection-hooks)))
  )

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
