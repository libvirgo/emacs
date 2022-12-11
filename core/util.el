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

(provide 'util)
