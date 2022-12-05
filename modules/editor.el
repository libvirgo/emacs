(setq el-get-sources
      '((:name awesome-pair :type github :pkgname "manateelazycat/awesome-pair")
        ))
(setq editor-require-packages
      (append
       '(awesome-pair
         )
       (mapcar 'el-get-as-symbol (mapcar 'el-get-source-name el-get-sources))))
(el-get 'sync editor-require-packages)

(use-package awesome-pair
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

(provide 'editor)
