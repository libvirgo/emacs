;;; -*- lexical-binding: t; -*-

(defun create-scratch-buffer nil
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))

(defun childframe-workable-p ()
  "Whether childframe is workable."
  (or (not (or noninteractive
               emacs-basic-display
               (not (display-graphic-p))))
      (daemonp)))

(defun project-buffer-p (buf)
  (member (buffer-name buf)
          (let ((buffers ()))
            (dolist (pr (project-buffers (project-current)))
              (push (buffer-name pr) buffers))
            buffers)))

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
					   "\*straight-byte-compilation\*"
					   "\*diff-hl-show-hunk-buffer\*"
					   "\*diff-hl-show-hunk-diff-buffer\*"
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

(defun copy-thing-at-point (thing)
  "Copy url at thing."
  (save-excursion
	(avy-goto-word-or-subword-1)
	(kill-new (thing-at-point thing))))

(provide 'clytie-util)
