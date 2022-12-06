(add-recipe-items
 '((:name vterm :type github :pkgname "akermu/emacs-libvterm")))
(setq term-require-packages
      (append
       '(vterm)
       (mapcar 'el-get-as-symbol (mapcar 'el-get-source-name el-get-sources))))
(el-get 'sync term-require-packages)

(provide 'term)


