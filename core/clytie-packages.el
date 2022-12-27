(eval-and-compile ; `use-package'
  (require  'use-package)
  (setq use-package-verbose t)
  (setq-default use-package-always-defer t))

(use-package epkg
  :config
  (setq epkg-repository (expand-file-name ".local/lib/epkgs" user-emacs-directory)))

(if is-darwin
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
    )

(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")

(defadvice load-theme (after run-after-load-theme-hook activate)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))

(provide 'clytie-packages)
