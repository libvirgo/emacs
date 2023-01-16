;;; -*- lexical-binding: t; -*-
(use-package tramp
  :straight (:type built-in)
  :init
  (setq tramp-persistency-file-name (expand-file-name "tramp" clytie-cache-dir)))

(use-package recentf
  :straight (:type built-in)
  :defer 3
  :config
  (setq recentf-save-file (expand-file-name "recentf" clytie-cache-dir))
  (recentf-mode))

(use-package autorevert
  :straight (:type built-in)
  :defer 3
  :config
  (global-auto-revert-mode))

(use-package electric-pair
  :straight (:type built-in)
  :hook (prog-mode . electric-pair-mode)
  :init
  (defvar electric-pair-inhibit-predicate-mode-chars-alist
  '((t . nil))
  "A list of major-mode and inhibit chars.

Each element is in the form of (MODE . (CHAR/CHAR-STRING/CHAR-FUNCTION ...)).

MODE
    A mode, or t for all modes.

CHAR
    A character to match the input. for example:

        ?\{

CHAR-STRING
    A pair of character and string, the character to match the input,
    the string for ‘looking-back’. for example:

        (?\{ . \":{\")

CHAR-FUNCTION
    A pair of character and function, the character to match the input,
    the function accept the input character as parameter. for example:

        (?\{ . (lambda (_c)
                 (eq ?: (char-before (1- (point))))))")

  (defun electric-pair-inhibit-predicate-function (c)
	(let ((alist
         (append
          (assoc-default major-mode electric-pair-inhibit-predicate-mode-chars-alist)
          (assoc-default t          electric-pair-inhibit-predicate-mode-chars-alist))))
      (or (cl-member c
					 alist
					 :test
					 (lambda (c it)
                       (cond
						((characterp it) (equal c it))
						((and (consp it) (equal c (car it)))
						 (cond ((stringp   (cdr it)) (looking-back (cdr it) 1))
                               ((functionp (cdr it)) (funcall (cdr it) c)))))))
          (electric-pair-default-inhibit c))))
  (with-eval-after-load 'elec-pair
	  (setq electric-pair-inhibit-predicate #'electric-pair-inhibit-predicate-function))
  )

(use-package hl-indent-scope
  :commands (hl-indent-scope-mode)
  :hook(
        (prog-mode . hl-indent-scope-mode)
        (after-load-theme . hl-indent-scope--auto-color-calc)))

(use-package hungry-delete
  :diminish
  :init (global-hungry-delete-mode)
  )

(use-package avy
  :bind (("s-l" . avy-goto-line)
         ("s-c" . avy-goto-char)
         ("s-f" . avy-goto-char-2)))

(setq-default tab-width 4)
;; (setq-default indent-tabs-mode nil)

(use-package posframe
  :init
  (defface posframe-border
    `((t (:inherit region)))
    "Face used by the `posframe' border."
    :group 'posframe)

  (with-eval-after-load 'persp-mode
    (add-hook 'persp-load-buffer-functions
              (lambda (&rest _)
                (posframe-delete-all))))
  :config
  (with-no-warnings
    (defun my-posframe--prettify-frame (&rest _)
      (set-face-background 'fringe nil posframe--frame))
    (advice-add #'posframe--create-posframe :after #'my-posframe--prettify-frame)

    (defun posframe-poshandler-frame-center-near-bottom (info)
      (cons (/ (- (plist-get info :parent-frame-width)
                  (plist-get info :posframe-width))
               2)
            (/ (+ (plist-get info :parent-frame-height)
                  (* 2 (plist-get info :font-height)))
               2)))))

(use-package ace-window
  :bind (("s-n" . ace-window)
		 ("C-x 1" . ace-delete-other-windows)
         )
  :config
  (setq aw-ignored-buffers '("*sort-tab*")
		aw-ignore-on t))

(use-package sort-tab
  :straight (:repo "manateelazycat/sort-tab" :type git :host github)
  :defer 3
  :bind (("s-1" . 'sort-tab-select-visible-tab)
         ("s-2" . 'sort-tab-select-visible-tab)
         ("s-3" . 'sort-tab-select-visible-tab)
         ("s-4" . 'sort-tab-select-visible-tab)
         ("s-5" . 'sort-tab-select-visible-tab)
         ("s-6" . 'sort-tab-select-visible-tab)
         ("s-7" . 'sort-tab-select-visible-tab)
         ("s-8" . 'sort-tab-select-visible-tab)
         ("s-9" . 'sort-tab-select-visible-tab)
         ("s-0" . 'sort-tab-select-visible-tab)
         ("s-[" . 'sort-tab-select-prev-tab)
         ("s-]" . 'sort-tab-select-next-tab)
		 ("s-q" . 'sort-tab-close-mode-tabs)
		 ("s-w" . 'sort-tab-close-current-tab)
		 ("C-." . fast-switch/body))
  :init
  (setq sort-tab-align 'center)
  :config
  (sort-tab-mode)
  (pretty-hydra-define fast-switch
	(:title "Switch" :color pink :quit-key ("q" "C-g"))
	("Move"
	 (("l" eyebrowse-next-window-config "switch next window")
	  ("h" eyebrowse-prev-window-config "switch prev window"))
	 "Tab"
	 (("C-a" sort-tab-select-first-tab "begin tab")
	  ("C-e" sort-tab-select-last-tab "end tab")
	  ("C-q" sort-tab-close-current-tab "close tab"))
	 "Search"
	 (("b" consult-buffer "search buffer" :color blue)
	  ("a" project-find-regexp "search project" :color blue))
	 "Window"
	 (("c" eyebrowse-create-window-config "create window")
	  ("d" eyebrowse-close-window-config "close window")
	  ("r" eyebrowse-rename-window-config "rename window"))
	 "Misc"
	 (("C-k" kill-current-buffer "kill buffer")
	  ("p" project-switch-project "switch project"))))
  )

;; (use-package awesome-tab
;;   :defer 3
;;   :bind (("s-1" . 'awesome-tab-select-visible-tab)
;;          ("s-2" . 'awesome-tab-select-visible-tab)
;;          ("s-3" . 'awesome-tab-select-visible-tab)
;;          ("s-4" . 'awesome-tab-select-visible-tab)
;;          ("s-5" . 'awesome-tab-select-visible-tab)
;;          ("s-6" . 'awesome-tab-select-visible-tab)
;;          ("s-7" . 'awesome-tab-select-visible-tab)
;;          ("s-8" . 'awesome-tab-select-visible-tab)
;;          ("s-9" . 'awesome-tab-select-visible-tab)
;;          ("s-0" . 'awesome-tab-select-visible-tab)
;;          ("s-[" . 'awesome-tab-backward-tab)
;;          ("s-]" . 'awesome-tab-forward-tab)
;;          )
;;   :config
;;   (awesome-tab-mode)
  ;; :bind
  ;; ("C-." . awesome-fast-switch/body))

(progn
  (when (treesit-available-p)
	(setq major-mode-remap-alist
        '((c-mode          . c-ts-mode)
          (c++-mode        . c++-ts-mode)
          (conf-toml-mode  . toml-ts-mode)
          (csharp-mode     . csharp-ts-mode)
          (css-mode        . css-ts-mode)
          (java-mode       . java-ts-mode)
          (js-mode         . js-ts-mode)
          (javascript-mode . js-ts-mode)
          (js-json-mode    . json-ts-mode)
          (python-mode     . python-ts-mode)
          (ruby-mode       . ruby-ts-mode)
          (sh-mode         . bash-ts-mode)
		  ))
	(add-to-list 'treesit-extra-load-path (expand-file-name "tree-sitter" clytie-local-dir)))
  )

(use-package hideshow
  :hook (prog-mode . hs-minor-mode)
  :bind
  (("M-[" . 'hs-hide-level)
   ("M-]" . 'hs-show-block)))

(provide 'init-editor)
