;;; -*- lexical-binding: t; -*-

(defconst prelude-user
  (getenv
   (if (equal system-type 'windows-nt) "USERNAME" "USER")))
(defconst is-darwin '(eq system-type 'darwin))
(defconst sys/linuxp
  (and (eq system-type 'gnu/linux)
       (not (string-match "-[Mm]icrosoft" operating-system-release)))
  "Are we running on a GNU/Linux system?")

(defconst prelude-dir (file-name-directory user-emacs-directory))
(defconst prelude-core-dir (expand-file-name "core" prelude-dir))
(defconst prelude-modules-dir (expand-file-name "modules" prelude-dir))
(defconst prelude-local-dir (expand-file-name "local" prelude-dir))
(defconst prelude-savefile-dir (expand-file-name "savefile" prelude-local-dir))
(defconst prelude-backup-dir (expand-file-name "backup" prelude-local-dir))
(defconst prelude-modules-file (expand-file-name "modules.el" prelude-modules-dir))

(defconst prelude-submodules-dir (expand-file-name "submodules" prelude-local-dir))

(defconst emacs-tmp-dir (expand-file-name (format "emacs%d" (user-uid)) prelude-savefile-dir))

(provide 'core-const)
