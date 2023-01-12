;;; -*- lexical-binding: t; -*-

;; (add-to-list 'load-path (expand-file-name ".local/lib/benchmark-init" user-emacs-directory))
;; (require 'benchmark-init-loaddefs)
;; (benchmark-init/activate)


(add-to-list 'load-path (expand-file-name "core" user-emacs-directory))

(require 'clytie-const)

(setq backup-directory-alist
        `((".*" . ,clytie-backup-dir)))
(setq auto-save-file-name-transforms
      `((".*" ,clytie-tmp-dir t)))
(setq auto-save-list-file-prefix
      clytie-tmp-dir)
(setq transient-levels-file (expand-file-name "transient/levels.el" clytie-cache-dir)
	  transient-values-file (expand-file-name "transient/values.el" clytie-cache-dir)
	  transient-history-file (expand-file-name "transient/history.el" clytie-cache-dir)
	  bookmark-default-file (expand-file-name "bookmarks" clytie-cache-dir)
	  make-backup-files nil
	  url-configuration-directory (expand-file-name "url" clytie-local-dir)
	  url-cookie-file (expand-file-name "cookies" url-configuration-directory)
	  url-cache-directory (expand-file-name "cache" url-configuration-directory))

(setq large-file-warning-threshold 100000000)
(setq load-prefer-newer t)

(require 'clytie-util)
(require 'clytie-packages)

(add-to-list 'load-path clytie-modules-dir)

(require 'modules)
