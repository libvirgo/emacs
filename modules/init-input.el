;;; -*- lexical-binding: t; -*-

;; (if sys/linuxp
;; 	  (use-package rime
;; 		:straight (:type built-in))
;;   (use-package rime
;; 	:straight (rime :type git
;; 					:host github
;; 					:repo "DogLooksGood/emacs-rime"
;; 					:files ("*.el" "Makefile" "lib.c"))))

;; (use-package rime
;;   :init
;;   (if sys/darwinp
;; 	  (progn
;; 		(setq
;; 		 rime-emacs-module-header-root "/opt/homebrew/Cellar/emacs-plus@29/29.0.60/include"
;;          rime-librime-root (expand-file-name "librime/dist" clytie-lib-dir)
;; 		 rime-user-data-dir "~/Library/Rime"
;; 		 ))
;; 	(progn
;; 	  (require 'rime)
;; 	  (setq rime-dir (expand-file-name "rime" clytie-cache-dir))
;; 	  (setq rime-user-data-dir (expand-file-name "deploy" rime-dir)
;; 			;; rime-share-data-dir (expand-file-name "config" rime-dir)
;; 			rime-share-data-dir "~/.config/ibus/rime"
;; 			)))
;;   (setq default-input-method "rime"
;;         rime-show-candidate 'posframe
;;         rime-posframe-style 'vertical)
;;   :bind (:map rime-mode-map ("s-j" . rime-inline-ascii)
;; 			  :map rime-mode-map ("M-j" . rime-force-enable))
;;   :config
;;   (setq rime-disable-predicates '(meow-normal-mode-p
;; 				                  meow-motion-mode-p
;; 				                  meow-keypad-mode-p
;; 				                  rime-predicate-prog-in-code-p
;; 				                  rime-predicate-after-alphabet-char-p))
;;   (setq rime-inline-predicates '(rime-predicate-space-after-cc-p
;; 				                 rime-predicate-current-uppercase-letter-p))
;;   )
(use-package pyim
  :init
  (setq default-input-method "pyim"
		pyim-page-length 7
		pyim-default-scheme 'xiaohe-shuangpin
		pyim-dcache-directory (expand-file-name "pyim/dcache/" clytie-cache-dir)
		pyim-page-tooltip 'posframe
		pyim-page-tooltip '(posframe popup minibuffer))
  (setq-default pyim-english-input-switch-functions
				'(pyim-probe-program-mode
				  meow-motion-mode-p
				  meow-normal-mode-p
				  meow-keypad-mode-p
				  pyim-probe-dynamic-english))
  (setq-default pyim-punctuation-translate-p '(no))
  (setq pyim-indicator-list (list #'pyim-indicator-with-cursor-color))
  :bind ("M-j" . pyim-convert-string-at-point)
  :config
  (use-package pyim-basedict
	:init
	(pyim-basedict-enable)
	)
  )

(provide 'init-input)
