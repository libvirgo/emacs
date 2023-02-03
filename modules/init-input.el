;;; -*- lexical-binding: t; -*-

(if sys/linuxp
	  (use-package rime
		:straight (:type built-in))
  (use-package rime
	:straight (rime :type git
					:host github
					:repo "DogLooksGood/emacs-rime"
					:files ("*.el" "Makefile" "lib.c"))))

(use-package rime
  :init
  (if sys/darwinp
	  (progn
		(setq
		 rime-emacs-module-header-root "/opt/homebrew/Cellar/emacs-plus@29/29.0.60/include"
         rime-librime-root (expand-file-name "librime/dist" clytie-lib-dir)
		 rime-user-data-dir "~/Library/Rime"
		 ))
	(progn
	  (require 'rime)
	  (setq rime-user-data-dir (expand-file-name "rime" clytie-cache-dir)
			rime-share-data-dir "~/.config/ibus/rime")))
  (setq default-input-method "rime"
        rime-show-candidate 'posframe
        rime-posframe-style 'vertical)
  :bind (:map rime-mode-map ("s-j" . rime-inline-ascii)
			  :map rime-mode-map ("M-j" . rime-force-enable))
  :config
  (setq rime-disable-predicates '(meow-normal-mode-p
				                  meow-motion-mode-p
				                  meow-keypad-mode-p
				                  rime-predicate-prog-in-code-p
				                  rime-predicate-after-alphabet-char-p))
  (setq rime-inline-predicates '(rime-predicate-space-after-cc-p
				                 rime-predicate-current-uppercase-letter-p))
  )

(provide 'init-input)
