;;; -*- lexical-binding: t; -*-

(defun create-scratch-buffer nil
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))

(defun childframe-workable-p ()
  "Whether childframe is workable."
  (or (not (or noninteractive
               emacs-basic-display
               (not (display-graphic-p))))
      (daemonp)))

(defun project-buffer-p (buf)
  (member (buffer-name buf)
          (let ((buffers ()))
            (dolist (pr (project-buffers (project-current)))
              (push (buffer-name pr) buffers))
            buffers)))

(provide 'core-util)
