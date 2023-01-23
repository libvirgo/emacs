;;; -*- lexical-binding: t; -*-

;; basic editor behavior
(require 'init-completion)
(require 'init-project)
(require 'init-vcs)
(require 'init-editor)
(require 'init-term)
(require 'init-org)
;; basic and global hydra keybind.
(require 'init-hydra)

;; lang
(require 'init-lsp)
(require 'init-golang)
(require 'init-rust)
(require 'init-protobuf)
(require 'init-elisp)
;; utils
(require 'init-input)
(require 'init-sql)
(require 'init-email)

;; end of editor
(require 'init-meow)
(require 'init-modeline)

(provide 'modules)
