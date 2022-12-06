(defun add-recipe-items (list-var)
  (dolist (recipe list-var)
    (add-to-list 'el-get-sources recipe)))

(provide 'util)
