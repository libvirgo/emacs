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
  :bind(("C-c g l" . avy-goto-line)
        ("C-c g c" . avy-goto-char)
        ("C-c g w" . avy-goto-char-2)))

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
  :bind (("C-c w w" . ace-window)
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
         ("C-c [" . 'awesome-tab-backward-tab)
         ("C-c ]" . 'awesome-tab-forward-tab)
         )
  :config
  (awesome-tab-mode)
  (with-eval-after-load 'hydra
      (defhydra awesome-fast-switch (:hint nil)
  "
 ^^^^Fast Move             ^^^^Tab                    ^^Search            ^^Misc
-^^^^--------------------+-^^^^---------------------+-^^----------------+-^^---------------------------
   ^_k_^   prev group    | _C-a_^^     select first | _b_ search buffer | _C-k_   kill buffer
 _h_   _l_  switch tab   | _C-e_^^     select last  | _g_ search group  | _C-S-k_ kill others in group
   ^_j_^   next group    | _C-j_^^     ace jump     | ^^                | ^_p_^ switch project
 _H_   _L_ switch eye    | _C-h_/_C-l_ move current | ^^                | ^^
 ^^0 ~ 9^^ eyebrowse     | ^^                     | ^^                | ^^
-^^^^--------------------+-^^^^---------------------+-^^----------------+-^^---------------------------
"
  ("h" awesome-tab-backward-tab)
  ("j" awesome-tab-forward-group)
  ("k" awesome-tab-backward-group)
  ("l" awesome-tab-forward-tab)
  ("L" eyebrowse-next-window-config)
  ("H" eyebrowse-prev-window-config)
  ("C-a" awesome-tab-select-beg-tab)
  ("C-e" awesome-tab-select-end-tab)
  ("C-j" awesome-tab-ace-jump)
  ("C-h" awesome-tab-move-current-tab-to-left)
  ("C-l" awesome-tab-move-current-tab-to-right)
  ("0" eyebrowse-switch-to-window-config-0)
  ("1" eyebrowse-switch-to-window-config-1)
  ("2" eyebrowse-switch-to-window-config-2)
  ("3" eyebrowse-switch-to-window-config-3)
  ("4" eyebrowse-switch-to-window-config-4)
  ("5" eyebrowse-switch-to-window-config-5)
  ("6" eyebrowse-switch-to-window-config-6)
  ("7" eyebrowse-switch-to-window-config-7)
  ("8" eyebrowse-switch-to-window-config-8)
  ("9" eyebrowse-switch-to-window-config-9)
  ("b" consult-buffer )
  ("g" awesome-tab-switch-group)
  ("C-k" kill-current-buffer)
  ("C-S-k" awesome-tab-kill-other-buffers-in-current-group)
  ("p" project-switch-project)
  ("q" nil "quit"))
  (global-set-key (kbd "C-.") 'awesome-fast-switch/body)
    )
  )

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

(use-package vundo
  :bind (("C-c u" . 'vundo))
  :config
  (setq undohist-directory (expand-file-name "undohist" clytie-cache-dir))
  (undohist-initialize))

(provide 'init-editor)
