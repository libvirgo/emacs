(defun prelude-el-get-check ()
  (defvar el-get-dir (expand-file-name "el-get" prelude-local-dir))
  (add-to-list 'load-path (expand-file-name "el-get" el-get-dir))
  (unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))
  (add-to-list 'el-get-recipe-path (expand-file-name "el-get/recipes" prelude-local-dir))
  (setq package-user-dir (expand-file-name "elpa" el-get-dir))
  )

(prelude-el-get-check)

(require 'cl-lib)

(setq prelude-require-package
      (append
       '(use-package
		 magit
		 smartparens
		 avy)
       (mapcar 'el-get-as-symbol (mapcar 'el-get-source-name el-get-sources))))

(el-get 'sync prelude-require-package)

(provide 'prelude-packages)
