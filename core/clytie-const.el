;;; -*- lexical-binding: t; -*-

(defconst clytie-user
  (getenv
   (if (equal system-type 'windows-nt) "USERNAME" "USER")))
(defconst is-darwin (eq system-type 'darwin))
(defconst sys/linuxp
  (and (eq system-type 'gnu/linux)
       (not (string-match "-[Mm]icrosoft" operating-system-release)))
  "Are we running on a GNU/Linux system?")

(defconst clytie-dir (file-name-directory user-emacs-directory))
(defconst clytie-core-dir (expand-file-name "core" clytie-dir))
(defconst clytie-modules-dir (expand-file-name "modules" clytie-dir))
(defconst clytie-local-dir (expand-file-name ".local" clytie-dir))
(defconst clytie-cache-dir (expand-file-name "cache" clytie-local-dir))
(defconst clytie-lib-dir (expand-file-name "lib" clytie-local-dir))
(defconst clytie-savefile-dir (expand-file-name "savefile" clytie-cache-dir))
(defconst clytie-backup-dir (expand-file-name "backup" clytie-cache-dir))
(defconst clytie-modules-file (expand-file-name "modules.el" clytie-modules-dir))
(defconst clytie-email-dir (expand-file-name "email" clytie-local-dir))
(defconst clytie-sql-dir (expand-file-name "ejc-sql" clytie-local-dir))

(defconst clytie-tmp-dir (expand-file-name (format "emacs%d" (user-uid)) clytie-savefile-dir))

(provide 'clytie-const)
