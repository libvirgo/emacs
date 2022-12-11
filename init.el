;;; -*- lexical-binding: t; -*-

;; (add-to-list 'load-path (expand-file-name "benchmark-init-el" prelude-local-dir))
;; (require 'benchmark-init-loaddefs)
;; (benchmark-init/activate)

(add-to-list 'load-path (expand-file-name "core" user-emacs-directory))
(require 'core-const)
(require 'core-util)
(require 'core)
(require 'core-packages)
(require 'core-editor)

(add-to-list 'load-path prelude-modules-dir)
(load prelude-modules-file)
