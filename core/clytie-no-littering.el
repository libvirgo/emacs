;;; -*- lexical-binding: t; -*-

(require 'clytie-const)
(setq backup-directory-alist
        `((".*" . ,clytie-backup-dir)))
(setq auto-save-file-name-transforms
      `((".*" ,clytie-tmp-dir t)))
(setq auto-save-list-file-prefix
      clytie-tmp-dir)
;; keeping user-emacs-directory clean.
(setq transient-levels-file (expand-file-name "transient/levels.el" clytie-cache-dir)
	  transient-values-file (expand-file-name "transient/values.el" clytie-cache-dir)
	  transient-history-file (expand-file-name "transient/history.el" clytie-cache-dir)
	  bookmark-default-file (expand-file-name "bookmarks" clytie-cache-dir)
	  make-backup-files nil
	  url-configuration-directory (expand-file-name "url" clytie-local-dir)
	  url-cookie-file (expand-file-name "cookies" url-configuration-directory)
	  url-cache-directory (expand-file-name "cache" url-configuration-directory))
(when sys/linuxp
  (eval-after-load 'x-win
      (let ((session-dir (expand-file-name "x-session" clytie-cache-dir)))
        `(progn
           (make-directory ,session-dir t)
           (defun emacs-session-filename (session-id)
             "Construct a filename to save the session in based on SESSION-ID.
This function overrides the one on `x-win' to use `no-littering'
directories."
             (expand-file-name session-id ,session-dir))))))

(provide 'clytie-no-littering)
