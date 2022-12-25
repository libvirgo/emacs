(defvar awesome-tray-git-buffer-filename "")
(defvar awesome-tray-git-command-cache "")
(defvar  awesome-tray-git-show-status t)
(defvar awesome-tray-git-format "[git:%s]")

(defun awesome-tray-git-command-update-cache ()
  (if (file-exists-p (format "%s" (buffer-file-name)))
      (let* ((filename (buffer-file-name))
             (status (vc-git-state filename))
             (branch (car (vc-git-branches))))

        (pcase status
          ('up-to-date (setq status ""))
          ('edited (setq status "!"))
          ('needs-update (setq status "⇣"))
          ('needs-merge (setq status "⇡"))
          ('unlocked-changes (setq status ""))
          ('added (setq status "+"))
          ('removed (setq status "-"))
          ('conflict (setq status "="))
          ('missing (setq status "?"))
          ('ignored (setq status ""))
          ('unregistered (setq status "?"))
          (_ (setq status "")))
        (if (not branch) (setq branch ""))

        (setq awesome-tray-git-buffer-filename filename)

        (setq awesome-tray-git-command-cache
              (if awesome-tray-git-show-status
                  (format awesome-tray-git-format (string-trim (concat branch " " status)))
                (format awesome-tray-git-format branch))))
    (setq awesome-tray-git-command-cache "")
    )
  )

(defun awesome-tray-module-git-info ()
  (if (executable-find "git")
      (progn
        (if (not (string= (buffer-file-name) awesome-tray-git-buffer-filename))
            (awesome-tray-git-command-update-cache))
        awesome-tray-git-command-cache)
    ""))

(defun +format-mode-line ()
  (let* ((lhs '(
                (:eval (when (bound-and-true-p meow-mode) (meow-indicator)))
                (:eval (if (fboundp 'rime-lighter) (format "%s "(rime-lighter)) ""))
                
                (:eval (propertize "%l" 'face 'font-lock-type-face))
                ","
                (:eval (propertize "%c" 'face 'font-lock-type-face))
                " "
                (:eval (eyebrowse-mode-line-indicator))
                ;; (:eval " L%l C%C")

                (:eval (when (bound-and-true-p flycheck-mode) flycheck-mode-line))
                (:eval (when (bound-and-true-p flymake-mode) flymake-mode-line-format))))
         (rhs '((:eval (awesome-tray-module-git-info))
                " "
                (:eval (propertize "%b" 'face 'font-lock-keyword-face 'help-echo (buffer-file-name)))
                " "
                (:eval mode-name)))
         (ww (window-width))
         (lhs-str (format-mode-line lhs))
         (rhs-str (format-mode-line rhs))
         (rhs-w (string-width rhs-str)))
    (format "%s%s%s"
            lhs-str
            (propertize " " 'display `((space :align-to (- (+ right right-fringe right-margin) (+ 1 ,rhs-w)))))
            rhs-str)))

(setq-default mode-line-format '((:eval (+format-mode-line))))
(setq-default header-line-format nil)

(provide 'init-modeline)