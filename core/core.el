;;; -*- lexical-binding: t; -*-

(setq backup-directory-alist
        `((".*" . ,prelude-backup-dir)))
(setq auto-save-file-name-transforms
      `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix
      emacs-tmp-dir)
(setq transient-levels-file (expand-file-name "transient/levels.el" prelude-local-dir))
(setq transient-values-file (expand-file-name "transient/values.el" prelude-local-dir))
(setq transient-history-file (expand-file-name "transient/history.el" prelude-local-dir))
(setq bookmark-default-file (expand-file-name "bookmarks" prelude-local-dir))

(setq large-file-warning-threshold 100000000)
(setq load-prefer-newer t)


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

(dolist (pack '(use-package diminish))
  (straight-use-package pack))

(if is-darwin
    (use-package exec-path-from-shell
      :defer 1
      :straight t
      :config
      (setq exec-path-from-shell-variables '("PATH" "PYTHONPATH" "GOPATH" "GTAGSOBJDIRPREFIX" "GTAGSCONF" "GTAGSLABEL"))
      (setq exec-path-from-shell-check-startup-files nil)
      (setq exec-path-from-shell-arguments '("-l"))
      (exec-path-from-shell-initialize))
    (when (fboundp 'set-fontset-font)
      (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend))
    (setq dired-use-ls-dired nil)
    )

(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")

(defadvice load-theme (after run-after-load-theme-hook activate)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))

(provide 'core)
