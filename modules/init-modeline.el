;;; -*- lexical-binding: t; -*-

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

(defun my/mode-line-mode-name ()
  (format "%s"
		  (propertize (format-mode-line mode-name)
					  'face '(:inherit font-lock-type-face))))

(use-package flymake
  :straight (:type built-in)
  :config
  (defun moon-flymake-mode-line ()
	(let* ((known (hash-table-keys flymake--state))
           (running (flymake-running-backends))
           (disabled (flymake-disabled-backends))
           (reported (flymake-reporting-backends))
           (diags-by-type (make-hash-table))
           (all-disabled (and disabled (null running)))
           (some-waiting (cl-set-difference running reported)))
      (maphash (lambda (_b state)
				 (mapc (lambda (diag)
						 (push diag
                               (gethash (flymake--diag-type diag)
										diags-by-type)))
                       (flymake--state-diags state)))
               flymake--state)
      (apply #'concat
			 (mapcar (lambda (args)
                       (apply (lambda (num str face)
								(propertize
								 (format str num) 'face face))
                              args))
					 `((,(length (gethash :error diags-by-type)) "[%d/" error)
					   (,(length (gethash :warning diags-by-type)) "%d] " warning)
                       )))))
  )

(defun +format-mode-line ()
  (let* ((lhs '(
                (:eval (when (bound-and-true-p meow-mode) (meow-indicator)))
                (:eval (propertize "%b" 'face 'font-lock-keyword-face 'help-echo (buffer-file-name)))
                (:eval (if (bound-and-true-p rime-mode) (if (fboundp 'rime-lighter) (format " %s "(rime-lighter)))
                         " "))
                (:eval (propertize "%l" 'face 'font-lock-type-face))
                ","
                (:eval (propertize "%c" 'face 'font-lock-type-face))
                ;; (:eval " L%l C%C")
                (:eval (when (bound-and-true-p flycheck-mode) flycheck-mode-line))
                (:eval (when (bound-and-true-p flymake-mode) (concat " " (moon-flymake-mode-line))))
                ))
         (rhs '(
                ;; (:eval minor-mode-alist) " "
                (:eval (eyebrowse-mode-line-indicator))
                " "
                (:eval (awesome-tray-module-git-info))
                " |"
                (:eval (concat (my/mode-line-mode-name)))
                ;; (:eval (propertize "%m" 'face 'font-lock-type-face))
                "|"
                ))
         (ww (window-width))
         (lhs-str (format-mode-line lhs))
         (rhs-str (format-mode-line rhs))
         (rhs-w (string-width rhs-str)))
    (format "%s%s%s"
            lhs-str
            (propertize " " 'display `((space :align-to (- (+ right right-fringe right-margin) (+ 1 ,rhs-w)))))
            rhs-str)))

(add-hook 'after-init-hook (lambda ()
                             (require 'vc-git)
                             (setq-default mode-line-format '((:eval (+format-mode-line))))
                             (setq-default header-line-format nil)))

(provide 'init-modeline)
