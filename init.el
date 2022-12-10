;;; -*- lexical-binding: t; -*-

(defconst prelude-user
  (getenv
   (if (equal system-type 'windows-nt) "USERNAME" "USER")))
(defconst is-darwin '(eq system-type 'darwin))
(defconst sys/linuxp
  (and (eq system-type 'gnu/linux)
       (not (string-match "-[Mm]icrosoft" operating-system-release)))
  "Are we running on a GNU/Linux system?")

(defconst prelude-dir (file-name-directory load-file-name))
(defconst prelude-core-dir (expand-file-name "core" prelude-dir))
(defconst prelude-modules-dir (expand-file-name "modules" prelude-dir))
(defconst prelude-local-dir (expand-file-name "local" prelude-dir))
(defconst prelude-savefile-dir (expand-file-name "savefile" prelude-local-dir))
(defconst prelude-backup-dir (expand-file-name "backup" prelude-local-dir))
(defconst prelude-modules-file (expand-file-name "modules.el" prelude-modules-dir))
(unless (file-exists-p prelude-local-dir)
  (make-directory prelude-local-dir)
  (unless (file-exists-p prelude-savefile-dir)
    (make-directory prelude-savefile-dir))
  (unless (file-exists-p prelude-backup-dir)
    (make-directory prelude-backup-dir)))
(defconst emacs-tmp-dir (expand-file-name (format "emacs%d" (user-uid)) prelude-savefile-dir))

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


(add-to-list 'load-path prelude-core-dir)
;; (add-to-list 'load-path (expand-file-name "benchmark-init-el" prelude-local-dir))
;; (require 'benchmark-init-loaddefs)
;; (benchmark-init/activate)

(require 'util)
(require 'core)
(require 'packages)
(require 'core-editor)

(add-to-list 'load-path prelude-modules-dir)
(load prelude-modules-file)
