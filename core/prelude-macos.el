(el-get 'sync 'exec-path-from-shell)

(use-package exec-path-from-shell
  :demand nil
  :config
  (setq exec-path-from-shell-variables '("PATH" "PYTHONPATH" "GOPATH" "GTAGSOBJDIRPREFIX" "GTAGSCONF" "GTAGSLABEL"))
  (setq exec-path-from-shell-check-startup-files nil)
  (setq exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-initialize))

(when (fboundp 'set-fontset-font)
  (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend))

(if is-darwin
    (setq dired-use-ls-dired nil))

(provide 'prelude-macos)
