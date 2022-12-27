;;; -*- lexical-binding: t; -*-

(use-package vterm
  :bind (:map vterm-mode-map
              ("C-'" . shell-pop-posframe-toggle))
  :init (setq vterm-always-compile-module t)
  (defvar shell-pop--frame nil)
  (defvar shell-pop--window nil)

  (defun shell-pop--shell (&optional arg)
    "Run shell and return the buffer."
    (cond ((fboundp 'vterm) (vterm arg))
          ((fboundp 'powershell) (powershell arg))
          (sys/win32p (eshell arg))
          (t (shell))))

  (defun shell-pop--hide-frame ()
    "Hide child frame and refocus in parent frame."
    (when (and (childframe-workable-p)
               (frame-live-p shell-pop--frame)
               (frame-visible-p shell-pop--frame))
      (make-frame-invisible shell-pop--frame)
      (select-frame-set-input-focus (frame-parent shell-pop--frame))
      (setq shell-pop--frame nil)))

  (defun shell-pop-toggle ()
    "Toggle shell."
    (interactive)
    (shell-pop--hide-frame)
    (if (window-live-p shell-pop--window)
        (progn
          (delete-window shell-pop--window)
          (setq shell-pop--window nil))
      (setq shell-pop--window
            (get-buffer-window (shell-pop--shell)))))

  (defun shell-pop-posframe-hidehandler (_)
    "Hidehandler used by `shell-pop-posframe-toggle'."
    (not (eq (selected-frame) shell-pop--frame)))

  (defun shell-pop-posframe-toggle ()
    "Toggle shell in child frame."
    (interactive)
    (let* ((buffer (shell-pop--shell))
           (window (get-buffer-window buffer)))
      ;; Hide window: for `popper'
      (when (window-live-p window)
        (delete-window window))

      (if (and (frame-live-p shell-pop--frame)
               (frame-visible-p shell-pop--frame))
          (progn
            ;; Hide child frame and refocus in parent frame
            (make-frame-invisible shell-pop--frame)
            (select-frame-set-input-focus (frame-parent shell-pop--frame))
            (setq shell-pop--frame nil))
        (let ((width  (max 100 (round (* (frame-width) 0.62))))
              (height (round (* (frame-height) 0.62))))
          ;; Shell pop in child frame
          (setq shell-pop--frame
                (posframe-show
                 buffer
                 :poshandler #'posframe-poshandler-frame-center
                 :hidehandler #'shell-pop-posframe-hidehandler
                 :left-fringe 8
                 :right-fringe 8
                 :width width
                 :height height
                 :min-width width
                 :min-height height
                 :internal-border-width 3
                 :internal-border-color (face-background 'posframe-border nil t)
                 :background-color (face-background 'tooltip nil t)
                 :override-parameters '((cursor-type . t))
                 :respect-mode-line t
                 :accept-focus t))

          ;; Focus in child frame
          (select-frame-set-input-focus shell-pop--frame)

          (with-current-buffer buffer
            (setq-local cursor-type 'box) ; blink cursor
            (goto-char (point-max))
            (when (fboundp 'vterm-reset-cursor-point)
              (vterm-reset-cursor-point)))))))
  (bind-key "C-'" #'shell-pop-posframe-toggle)
  )

<<<<<<< HEAD

(provide 'init-term)


=======
(provide 'init-term)
>>>>>>> borg
