;;; -*- lexical-binding: t; -*-

;; (add-to-list 'load-path (expand-file-name ".local/lib/benchmark-init" user-emacs-directory))
;; (require 'benchmark-init-loaddefs)
;; (benchmark-init/activate)

(setq large-file-warning-threshold 100000000)
(setq load-prefer-newer t)

(add-to-list 'load-path (expand-file-name "core" user-emacs-directory))
(require 'clytie-no-littering)
(require 'clytie-util)
(require 'clytie-packages)

(add-to-list 'load-path clytie-modules-dir)

(require 'modules)
