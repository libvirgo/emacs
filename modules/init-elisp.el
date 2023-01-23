;;; -*- lexical-binding: t; -*-

(use-package emacs-lisp
  :straight (:type built-in)
  :hook
  ((emacs-lisp-mode . (lambda ()
                        (setq-local completion-at-point-functions
                                    (list (cape-super-capf
                                           #'elisp-completion-at-point
                                           #'cape-dabbrev)))))))

(provide 'init-elisp)
