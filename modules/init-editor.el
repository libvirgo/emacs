;;; -*- lexical-binding: t; -*-

(use-package electric-pair
  :hook (after-init . electric-pair-mode))

(use-package hl-indent-scope
  :commands (hl-indent-scope-mode)
  :hook(
        (prog-mode . hl-indent-scope-mode)
        (after-load-theme . hl-indent-scope--auto-color-calc)))

(use-package hungry-delete
  :diminish
  :init (global-hungry-delete-mode)
  )

(use-package smartparens)

(use-package avy
  :bind(("C-c g l" . avy-goto-line)
        ("C-c g c" . avy-goto-char)
        ("C-c g w" . avy-goto-char-2)))

(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

(defun vmacs-boring-buffer-p(&optional buf)
  (string-match-p (rx (or
                       "\*Async-native-compile-log\*"
                       "magit"
                       "\*company-documentation\*"
                       "\*eaf" "\*eldoc" "\*Launch " "*dap-"
                       "*EGLOT " "\*Flymake log\*"
                       "\*gopls::stderr\*" "\*gopls\*"
                       "\*Compile-Log\*" "*Backtrace*"
                       "*Package-Lint*" "\*sdcv\*" "\*tramp"
                       "\*lsp-log\*" "\*tramp" "\*Ibuffer\*"
                       "\*Help\*" "\*ccls" "\*vc"
                       "\*xref" "\*Warnings*" "\*Http"
                       "\*Async Shell Command\*"
                       "\*Shell Command Output\*"
                       "\*Calculator\*" "\*Calc "
                       "\*Flycheck error messages\*"
                       "\*Gofmt Errors\*"
                       "\*Ediff" "\*sdcv\*"
                       "\*Org PDF LaTex Output\*"
                       "\*Org Export"
                       "\*osx-dictionary\*" "\*Messages\*"
                       "\*straight-process\*"
                       ))
                  (buffer-name buf)))

(defun vmacs-tab-vterm-p(&optional buf)
  (eq (buffer-local-value 'major-mode (or buf (current-buffer))) 'vterm-mode))

;; switch-to-prev-buffer 与 switch-to-next-buffer 时 skip 特定的buffers
;;而 tab-line-switch-to-prev/next-tab 恰好使用了上面两个函数
(defun vmacs-switch-to-prev-buffer-skip(win buf bury-or-kill)
  (when (member this-command '(next-buffer previous-buffer
                                           switch-to-prev-buffer
                                           switch-to-next-buffer))
    (cond
     ;; ((vmacs-tab-vterm-p)                ;当前buffer是vterm
     ;; (not (vmacs-tab-vterm-p buf)))     ;若buf 不是vterm,则skip
     ((vmacs-boring-buffer-p (current-buffer))
      (not (vmacs-boring-buffer-p buf)))
     (t                                 ;当前buffer是正常buffer
      (or (vmacs-boring-buffer-p buf)   ;若buf 是boring buf 或vterm，则跳过
          (vmacs-tab-vterm-p buf)
          (not (project-buffer-p buf))
          )))))

(setq switch-to-prev-buffer-skip #'vmacs-switch-to-prev-buffer-skip)

(use-package posframe
  :hook (after-load-theme . posframe-delete-all)
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

(provide 'init-editor)
