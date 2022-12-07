(defun create-scratch-buffer nil
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))

(provide 'util)
