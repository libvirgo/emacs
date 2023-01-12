;;; -*- lexical-binding: t; -*-

(setq doom-gc-cons-threshold 524288000)
(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6)

(add-hook 'focus-out-hook #'garbage-collect)

(defvar doom--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
;; Alternatively, restore it even later:
(add-hook 'emacs-startup-hook
  (lambda ()
    (setq file-name-handler-alist doom--file-name-handler-alist)))

(setq package-enable-at-startup nil ; don't auto-initialize!
      ;; don't add that `custom-set-variables' block to my init.el!
      package--init-file-ensured t)

(setq load-prefer-newer noninteractive)

(setq frame-inhibit-implied-resize t)

(load (expand-file-name "core/core" user-emacs-directory))
