(defvar prelude-user
  (getenv
   (if (equal system-type 'windows-nt) "USERNAME" "USER")))

(setq load-prefer-newer t)

(defvar prelude-dir (file-name-directory load-file-name))
(defvar prelude-core-dir (expand-file-name "core" prelude-dir))
(defvar prelude-modules-dir (expand-file-name "modules" prelude-dir))

(defvar prelude-local-dir (expand-file-name "local" prelude-dir))

(defvar prelude-savefile-dir (expand-file-name "savefile" prelude-local-dir))
(defvar prelude-backup-dir (expand-file-name "backup" prelude-local-dir))
(defvar prelude-modules-file (expand-file-name "prelude-modules.el" prelude-modules-dir))
(defvar is-darwin '(eq system-type 'darwin))

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

(add-to-list 'load-path prelude-core-dir)
(add-to-list 'load-path prelude-modules-dir)

(require 'prelude-packages)
(require 'prelude-ui)
(require 'prelude-editor)
(require 'core)
(setq large-file-warning-threshold 100000000)

(use-package project
  :init
  (setq project-list-file (expand-file-name "projects" prelude-local-dir)))
(use-package eshell
  :init
  (setq eshell-directory-name (expand-file-name "eshell" prelude-local-dir)))
(load prelude-modules-file)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(go-fill-struct orderless compat)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
