(defun clytie-straight-check ()
  (defvar bootstrap-version)
  (setq straight-use-package-by-default t)
  (setq straight-vc-git-default-clone-depth 1)
  (setq straight-base-dir clytie-local-dir)
  (setq straight-repository-user "libvirgo")
  (if (and (executable-find "watchexec")
           (executable-find "python3"))
      (setq straight-check-for-modifications '(watch-files find-when-checking))
    (setq straight-check-for-modifications
          '(find-at-startup find-when-checking)))
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
    (load bootstrap-file nil 'nomessage)))

(clytie-straight-check)

(dolist (pack '(use-package diminish))
  (straight-use-package pack))

(setq use-package-verbose t)
(setq-default use-package-always-defer t)

(when is-darwin
  (use-package exec-path-from-shell
    :defer 1
    :config
    (setq exec-path-from-shell-variables '("PATH" "PYTHONPATH" "GOPATH" "GTAGSOBJDIRPREFIX" "GTAGSCONF" "GTAGSLABEL"))
    (setq exec-path-from-shell-check-startup-files nil)
    (setq exec-path-from-shell-arguments '("-l"))
    (exec-path-from-shell-initialize))
  
  (when (fboundp 'set-fontset-font)
    (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend))
  (setq dired-use-ls-dired nil)
  ;; stop accidental text scaling from brushing trackpad in GNU Emacs on macOS
  (defvar using-trackpad-timer nil)
  (defvar using-trackpad nil)
  (defun mouse-present-p ()
    (with-temp-buffer
      (call-process "ioreg" nil (current-buffer) nil "-p" "IOUSB")
      (goto-char (point-min))
      (and (search-forward "USB Receiver" nil t) t)))
  
  (defun set-using-trackpad ()
    (setq using-trackpad (not (mouse-present-p))))
  
  (defun maybe-mouse-wheel-text-scale (event)
    (interactive (list last-input-event))
    (when (not using-trackpad)
      (mouse-wheel-text-scale event)))
  
  (when using-trackpad-timer
    (cancel-timer using-trackpad-timer))
  (setq using-trackpad-timer (run-at-time "0" 60 'set-using-trackpad))
  (global-set-key [(control wheel-up)] 'maybe-mouse-wheel-text-scale)
  (global-set-key [(control wheel-down)] 'maybe-mouse-wheel-text-scale))

(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")

(defadvice load-theme (after run-after-load-theme-hook activate)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))

(provide 'clytie-packages)
