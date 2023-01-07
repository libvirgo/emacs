;;; -*- lexical-binding: t; -*-
(use-package tramp
  :init
  (setq tramp-persistency-file-name (expand-file-name "tramp" clytie-cache-dir)))

(use-package recentf
  :defer 3
  :config
  (setq recentf-save-file (expand-file-name "recentf" clytie-cache-dir))
  (recentf-mode))

(use-package autorevert
  :defer 3
  :config
  (global-auto-revert-mode))

(use-package electric-pair
  :hook (prog-mode . electric-pair-mode))

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
  (add-hook 'after-load-theme-hook 'posframe-delete-all)
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
         ))

(use-package awesome-tab
  :defer 3
  :bind (("s-1" . 'awesome-tab-select-visible-tab)
         ("s-2" . 'awesome-tab-select-visible-tab)
         ("s-3" . 'awesome-tab-select-visible-tab)
         ("s-4" . 'awesome-tab-select-visible-tab)
         ("s-5" . 'awesome-tab-select-visible-tab)
         ("s-6" . 'awesome-tab-select-visible-tab)
         ("s-7" . 'awesome-tab-select-visible-tab)
         ("s-8" . 'awesome-tab-select-visible-tab)
         ("s-9" . 'awesome-tab-select-visible-tab)
         ("s-0" . 'awesome-tab-select-visible-tab)
         ("s-[" . 'awesome-tab-backward-tab)
         ("s-]" . 'awesome-tab-forward-tab)
         )
  :config
  (awesome-tab-mode)
  (pretty-hydra-define awesome-fast-switch
	(:title "Switch" :color pink :quit-key ("q" "C-g"))
	("Move"
	 (("h" awesome-tab-backward-tab "backward tab")
	  ("j" awesome-tab-forward-group "forward tab group")
	  ("k" awesome-tab-backward-group "backward tab group")
	  ("l" awesome-tab-forward-tab "forward tab")
	  ("L" eyebrowse-next-window-config "switch next window")
	  ("H" eyebrowse-prev-window-config "switch prev window"))
	 "Tab"
	 (("C-a" awesome-tab-select-beg-tab "begin tab")
	  ("C-e" awesome-tab-select-end-tab "end tab")
	  ("C-j" awesome-tab-ace-jump "tab ace jump")
	  ("C-h" awesome-tab-move-current-tab-to-left "move tab left")
	  ("C-l" awesome-tab-move-current-tab-to-right "move tab right"))
	 "Search"
	 (("b" consult-buffer "search buffer" :color blue)
	  ("g" awesome-tab-switch-group "switch tab group" :color blue))
	 "Window"
	 (("c" eyebrowse-create-window-config "create window")
	  ("d" eyebrowse-close-window-config "close window")
	  ("r" eyebrowse-rename-window-config "rename window"))
	 "Misc"
	 (("C-k" kill-current-buffer "kill buffer")
	  ("C-S-k" awesome-tab-kill-other-buffers-in-current-group "kill other buffers")
	  ("p" project-switch-project "switch project"))))
  :bind
  ("C-." . awesome-fast-switch/body))

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

(progn
  (keymap-global-set "s-w" #'kill-this-buffer))

(use-package hideshow
  :hook (prog-mode . hs-minor-mode)
  :bind
  (("M-[" . 'hs-hide-level)
   ("M-]" . 'hs-show-block)))

(provide 'init-editor)
