;;; -*- lexical-binding: t; -*-

(use-package nix-mode
  :straight (:files ("nix*.el"))
  :mode "\\.nix\\'"
  :hook ((nix-mode . (lambda ()
										 (setq-local completion-at-point-functions (list (cape-super-capf
																						  (cape-company-to-capf #'company-nix)
																						  ;; #'cape-dabbrev
																						  ))))))
)

(provide 'init-nix)
