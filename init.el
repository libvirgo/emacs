;;; -*- lexical-binding: t; -*-

(add-to-list 'load-path (expand-file-name "core" user-emacs-directory))

(require 'clytie-const)

(setq backup-directory-alist
        `((".*" . ,clytie-backup-dir)))
(setq auto-save-file-name-transforms
      `((".*" ,clytie-tmp-dir t)))
(setq auto-save-list-file-prefix
      clytie-tmp-dir)
(setq transient-levels-file (expand-file-name "transient/levels.el" clytie-cache-dir))
(setq transient-values-file (expand-file-name "transient/values.el" clytie-cache-dir))
(setq transient-history-file (expand-file-name "transient/history.el" clytie-cache-dir))
(setq bookmark-default-file (expand-file-name "bookmarks" clytie-cache-dir))
(setq make-backup-files nil)

(setq large-file-warning-threshold 100000000)
(setq load-prefer-newer t)

(eval-and-compile ; `borg'
  (add-to-list 'load-path (expand-file-name ".local/lib/borg" user-emacs-directory))
  (require 'borg)
  (borg-initialize))


(require 'clytie-util)
(require 'clytie-packages)

(add-to-list 'load-path clytie-modules-dir)

(require 'modules)
