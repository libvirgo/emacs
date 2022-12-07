(add-recipe-items
 '((:name cape :type github :pkgname "minad/cape")
   (:name citre :type github :pkgname "universal-ctags/citre")
   (:name orderless :type github :pkgname "oantolin/orderless")
   (:name kind-icon :type github :pkgname "jdtsmith/kind-icon")
   (:name eglot :type github :pkgname "joaotavora/eglot")
   (:name corfu-english-helper :type github :pkgname "manateelazycat/corfu-english-helper")
   ))



(setq completion-require-packages
      (append
       '(corfu
         cape
         citre
         orderless
         eglot
         kind-icon
         corfu-english-helper)
       (mapcar 'el-get-as-symbol (mapcar 'el-get-source-name el-get-sources))))
(el-get 'sync completion-require-packages)

(use-package kind-icon
  :defer 1
  :custom
  (kind-icon-use-icons nil)
  (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
  )

(use-package corfu
  :bind ((:map corfu-map ("SPC" . corfu-insert-separator)))
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-separator ?\s)          ;; Orderless field separator
  (corfu-auto-delay 0)
  (corfu-quit-no-match 'separator)
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect-first nil)    ;; Disable candidate preselection
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-excluded-modes'.

  (corfu-history-mode)
  (savehist-mode)
  (add-to-list 'savehist-additional-variables 'corfu-history)
  :init
  (global-corfu-mode)
  (corfu-history-mode))

(use-package cape
  :bind (
         ("C-c c p" . completion-at-point))
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  )

(use-package dabbrev
  :bind (("C-c c d" . dabbrev-completion)))

(use-package orderless
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
  :bind (:map eglot-mode-map
              ("C-c C-l C-a" . eglot-code-actions)
              ("C-c C-l C-r" . eglot-rename)
              ))

(use-package corfu-english-helper
  :bind (
         ("C-h C-e" . corfu-english-helper-search)))

(provide 'completion)
