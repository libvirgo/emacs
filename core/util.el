(defun add-recipe-items (list-var)
  (dolist (recipe list-var)
    (add-to-list 'el-get-sources recipe)))

(defun create-scratch-buffer nil
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))

(provide 'util)
