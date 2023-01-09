;;; -*- lexical-binding: t; -*-

(use-package protobuf-mode
  :init
  (setq auto-mode-alist  (cons '(".proto$" . protobuf-mode) auto-mode-alist))
  (defconst my-protobuf-style
    '((c-basic-offset . 2)
      (indent-tabs-mode . nil)))
  (add-hook 'protobuf-mode-hook
			(lambda () (c-add-style "my-style" my-protobuf-style t)))
  )

(provide 'init-protobuf)
