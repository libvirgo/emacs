;;; -*- lexical-binding: t; -*-

(use-package hydra
  :hook (after-init . (lambda () (require 'hydra))))

(use-package major-mode-hydra
  :hook (after-init . (lambda () (require 'major-mode-hydra))))

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
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ;; ("C-c m" . consult-mode-command)
         ;; ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ;; ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
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
         ("C-c s m" . consult-multi-occur)
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
         ;; ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

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
  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config
  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
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
  :hook
  (embark-collect-mode . consult-preview-at-point-mode)
  :bind
  (("C-;" . embark-act)
   ("M-." . embark-dwim)
   ("C-h B" . embark-bindings))
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

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
  :bind ((:map corfu-map
               ("C-SPC" . corfu-insert-separator)
               ("<tab>" . corfu-complete-common-or-next)
               ("C-f" . corfu-quick-insert)))
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-separator ?\s)          ;; Orderless field separator
  (corfu-auto-delay 0)
  (corfu-quit-no-match t)
  (corfu-quit-at-boundary t)
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
  (global-corfu-mode))

;; yasnippet replacement.
(use-package tempel
  :custom
  (tempel-trigger-prefix "<")
  :bind (:map tempel-map
              ("C-f" . tempel-next)
              ("C-b" . tempel-previous)
              ("C-a" . tempel-beginning)
              ("RET" . tempel-done)
              )
  :init
  ;; Setup completion at point
  (defun tempel-setup-capf ()
    (setq-local completion-at-point-functions
                (cons #'tempel-complete
                      completion-at-point-functions)))
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)
  (setq-default tempel-path (expand-file-name "tempel-collection/templates/*.eld" clytie-lib-dir))
  )

(use-package cape
  :bind (
         ("C-c c p" . completion-at-point)
         ("C-c c d" . cape-dabbrev)
         ("C-c c f" . cape-file))
  :hook
  ((emacs-lisp-mode . (lambda ()
                        (setq-local completion-at-point-functions
                                    (list (cape-super-capf
                                           #'tempel-complete
                                           #'elisp-completion-at-point
                                           #'cape-dabbrev))))))
  :init
  (setq cape-dabbrev-min-length 1)
  (setq cape-dabbrev-check-other-buffers t)
  ;; (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  )

;; for some company-backend.
(use-package company)

(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;; orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '(
                                        (file (styles partial-completion))
                                        (eglot (styles orderless))))
  :config
  (defun without-if-bang (pattern _index _total)
    (cond
     ((equal "!" pattern)
      '(orderless-literal . ""))
     ((string-prefix-p "!" pattern)
      `(orderless-without-literal . ,(substring pattern 1)))))
  (add-to-list 'orderless-style-dispatchers #'without-if-bang))

(use-package eglot
  :init
  (with-eval-after-load 'embark
      (push 'embark--allow-edit
            (alist-get 'eglot-rename embark-target-injection-hooks)))
  )

(use-package corfu-english-helper
  :straight (corfu-english-helper :type git :host github :repo "manateelazycat/corfu-english-helper" :feature orfu-english-helper-data)
  :bind (
         ("C-h C-e" . corfu-english-helper-search)))

(use-package citre
  :bind (
         ("C-c e d" . citre-peek)
         ("C-c e r" . citre-peek-reference)
         ("C-c e b" . citre-jump-back)
         ("C-c e g" . citre-peek-restore)
         :map citre-peek-keymap
         ("C-j" . citre-peek-jump)
         ("C-t" . citre-peek-through)
         ("C-u" . citre-peek-through-reference)
         ("C-n" . citre-peek-next-line)
         ("C-p" . citre-peek-prev-line)
         ("M-n" . citre-peek-next-tag)
         ("M-p" . citre-peek-prev-tag)
         ("C-f" . citre-peek-chain-forward)
         ("C-b" . citre-peek-chain-backward)
         )
  )

(provide 'init-completion)
