;;; -*- lexical-binding: t; -*-

(use-package xref
  :init
  (setq xref-search-program 'ripgrep))

(use-package vertico
  :init (vertico-mode)
  )

(use-package marginalia
  :hook (after-init . marginalia-mode))

;; Example configuration for Consult
(use-package consult
  ;; The :init configuration is always executed (Not lazy)
  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  (advice-add #'project-find-regexp :override #'consult-ripgrep)
  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ;; ("C-c m" . consult-mode-command)
         ;; ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ;; ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
		 ("s-b" . consult-project-buffer)
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-othecompletionr-frame
         ("C-c b b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-c b d" . bookmark-delete)
         ("C-c b m" . bookmark-set)
         ("C-c b s" . bookmark-save)
         ;; Custom M-# bindings for fast register access
         ("C-c r i" . consult-register-load)
         ("C-c r s" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-c r r" . consult-register)
         ;; Other custom bindings
         ("C-c r y" . consult-yank-pop)                ;; orig. yank-pop
         ("M-y" . consult-yank-pop)
         ;; M-g bindings (goto-map)
         ("C-c g e" . consult-compile-error)
         ("C-c g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("s-g" . consult-goto-line)
         ("C-c g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("C-c m f" . mark-defun)
         ("C-c m p" . mark-page)
         ("C-c m j" . consult-mark)
         ("C-c m J" . consult-global-mark)
         ("C-c c i" . consult-imenu)
         ("C-c c I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ;; ("M-s D" . consult-locate)
         ("C-c s l" . consult-line)
         ("C-c s L" . consult-line-multi)
         ("C-c s r" . consult-ripgrep)
         ("C-c s u" . consult-focus-lines)
         ;; Isearch integration
         ("C-c s h" . consult-isearch-history)
         :map isearch-mode-map
         ("C-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("C-l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("C-m" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("C-h" . consult-history))                 ;; orig. next-matching-history-element
  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key (kbd "M-.")
   :preview-key '(:debounce 0.4 any))
  (defvar consult--fd-command nil)
  (defun consult--fd-builder (input)
    (unless consult--fd-command
      (setq consult--fd-command
            (if (eq 0 (call-process-shell-command "fdfind"))
                "fdfind"
              "fd")))
    (pcase-let* ((`(,arg . ,opts) (consult--command-split input))
                 (`(,re . ,hl) (funcall consult--regexp-compiler
                                        arg 'extended t)))
      (when re
        (list :command (append
                        (list consult--fd-command
                              "--color=never" "--full-path"
                              (consult--join-regexps re 'extended))
                        opts)
              :highlight hl))))
  (defun consult-fd (&optional dir initial)
    (interactive "P")
    (let* ((prompt-dir (consult--directory-prompt "Fd" dir))
           (default-directory (cdr prompt-dir)))
      (find-file (consult--find (car prompt-dir) #'consult--fd-builder initial))))
  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")
  )

(use-package embark
  :straight (embark :files ("embark.el" "embark-consult.el" "embark-org.el"))
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode)
  :bind
  (("C-;" . embark-act)
   ("M-." . embark-dwim)
   ("C-h B" . embark-bindings))
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  )

(use-package wgrep)

(use-package kind-icon
  :defer 3
  :custom
  (kind-icon-use-icons nil)
  (kind-icon-default-face 'corfu-default)
  :config
  (with-eval-after-load 'corfu
    (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
    (add-hook 'after-load-theme-hook #'kind-icon-reset-cache)
    )
  )

(use-package corfu
  :straight (corfu :type git :host github :repo "minad/corfu" :files ("corfu.el" "extensions/corfu-quick.el"))
  :init
  (defun corfu-complete-common-or-next ()
    "Complete common prefix or go to next candidate."
    (interactive)
    (if (= corfu--total 1)
        (progn
          (corfu--goto 1)
          (corfu-insert))
      (let* ((input (car corfu--input))
             (str (if (thing-at-point 'filename) (file-name-nondirectory input) input))
             (pt (length str))
             (common (try-completion str corfu--candidates)))
        (if (and (> pt 0)
                 (stringp common)
                 (not (string= str common)))
            (insert (substring common pt))
          (corfu-next)))))
  (global-corfu-mode)
  :bind ((:map corfu-map
               ("C-SPC" . corfu-insert-separator)
               ("C-n" . corfu-complete-common-or-next)
               ("C-f" . corfu-quick-insert)))
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-separator ?\s)          ;; Orderless field separator
  (corfu-auto-delay 0)
  (corfu-quit-no-match t)
  (corfu-quit-at-boundary 'separator)
  (corfu-popupinfo-delay '(1.0 . 1.0)))

(use-package emacs
  :init
  (setq tab-always-indent 'complete))

(use-package yasnippet-snippets
  :straight (:host github :type git :repo "AndreaCrotti/yasnippet-snippets" :depth full :files ("yasnippet-snippets.el" "snippets") :fork (:repo "libvirgo" :protocol ssh)))

(use-package yasnippet
  :init
  (setq yas-snippet-dirs nil)
  :bind (:map yas-keymap
			  ("C-c C-n" . yas-next-field)
			  ("C-c C-p" . yas-prev-field))
  :config
  (setq yas--default-user-snippets-dir nil)
  (yasnippet-snippets-initialize))

(use-package consult-yasnippet
  :init
  (advice-add 'consult-yasnippet :before (lambda (&rest _)
										   (unless (bound-and-true-p yas-minor-mode)
											 (yas-minor-mode-on))))
  :bind (("C-c C-y" . consult-yasnippet)))

;; for some company-backend.
(use-package company)

(use-package cape
  :init
  (setq cape-dabbrev-min-length 1)
  (setq cape-dabbrev-check-other-buffers t)
  ;; (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  :bind (
         ("C-c c d" . cape-dabbrev)
         ("C-c c f" . cape-file))
  )

(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;; orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '(
                                        (file (styles partial-completion))))
  :config
  (defun without-if-bang (pattern _index _total)
    (cond
     ((equal "!" pattern)
      '(orderless-literal . ""))
     ((string-prefix-p "!" pattern)
      `(orderless-without-literal . ,(substring pattern 1)))))
  (add-to-list 'orderless-style-dispatchers #'without-if-bang))



(use-package corfu-english-helper
  :straight (corfu-english-helper :type git :host github :repo "manateelazycat/corfu-english-helper" :feature orfu-english-helper-data)
  :bind (
         ("C-h C-e" . corfu-english-helper-search)))

(use-package sdcv
  :straight (sdcv :host github :repo "manateelazycat/sdcv")
  :init
  (setq sdcv-only-data-dir t)
  (setq sdcv-dictionary-data-dir (expand-file-name "dict" clytie-local-dir))
  (setq sdcv-dictionary-simple-list    ;setup dictionary list for simple search
		'(
        "懒虫简明英汉词典"
        ))
  :bind (("C-h C-w" . sdcv-search-input+)))

(use-package multi-translate
  :straight (multi-translate :host github :repo "twlz0ne/multi-translate.el")
  :bind (("C-h C-t" . multi-translate-at-point)
		 :map multi-translate-mode-map
		 ("p" . multi-translate-prev-translation-section)
		 ("n" . multi-translate-next-translation-section)
		 ("c" . multi-translate-clean-buffer))
  :custom
  (multi-translate-word-backends nil))

(use-package citre
  :bind (
         :map citre-peek-keymap
         ("C-j" . citre-peek-jump)
         ("C-t" . citre-peek-through)
         ("C-u" . citre-peek-through-reference)
         ("C-n" . citre-peek-next-line)
         ("C-p" . citre-peek-prev-line)
         ("M-n" . citre-peek-next-tag)
         ("M-p" . citre-peek-prev-tag)
         ("M-f" . citre-peek-chain-forward)
         ("M-b" . citre-peek-chain-backward)
         )
  )

(provide 'init-completion)
