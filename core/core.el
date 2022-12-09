;;; -*- lexical-binding: t; -*-

(defun prelude-straight-check ()
  (defvar bootstrap-version)
  (setq-default straight-use-package-by-default t)
  (setq-default straight-vc-git-default-clone-depth 1)
  (setq-default straight-base-dir prelude-local-dir)
  (let ((repo-dir (expand-file-name "straight/repos/straight.el" straight-base-dir))
        ))
  (let ((bootstrap-file
         (expand-file-name "straight/repos/straight.el/bootstrap.el" straight-base-dir))
        (bootstrap-version 6))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))
  )

(prelude-straight-check)
(setq-default use-package-always-defer t)

(straight-use-package 'use-package)
(straight-use-package 'delight)
(straight-use-package 'diminish)
(if is-darwin
    (use-package exec-path-from-shell
      :straight t
      :config
      (setq exec-path-from-shell-variables '("PATH" "PYTHONPATH" "GOPATH" "GTAGSOBJDIRPREFIX" "GTAGSCONF" "GTAGSLABEL"))
      (setq exec-path-from-shell-check-startup-files nil)
      (setq exec-path-from-shell-arguments '("-l"))
      (exec-path-from-shell-initialize))

  (when (fboundp 'set-fontset-font)
    (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend))
  )

(provide 'core)
