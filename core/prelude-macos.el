(el-get-bundle exec-path-from-shell)
(setq exec-path-from-shell-variables '("PATH" "PYTHONPATH" "GOPATH" ))
(setq exec-path-from-shell-check-startup-files nil)
(setq exec-path-from-shell-arguments '("-l"))
(exec-path-from-shell-initialize)
(when (fboundp 'set-fontset-font)
  (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend))

(provide 'prelude-macos)
