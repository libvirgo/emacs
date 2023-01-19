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
         )
  :config)

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

(progn
  (keymap-global-set "s-w" #'kill-current-buffer)
  (keymap-global-set "s-[" #'switch-to-prev-buffer)
  (keymap-global-set "s-]" #'switch-to-next-buffer))

(provide 'init-editor)
