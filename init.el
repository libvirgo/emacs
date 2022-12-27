(eval-and-compile ; `borg'
  (add-to-list 'load-path (expand-file-name ".local/lib/borg" user-emacs-directory))
  (require 'borg)
  (borg-initialize))

(setq epkg-repository (expand-file-name ".local/lib/epkgs" user-emacs-directory))

(eval-and-compile ; `use-package'
  (require  'use-package)
  (setq use-package-verbose t)
  (setq-default use-package-always-defer t))

