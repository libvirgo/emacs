;;; -*- lexical-binding: t; -*-

(use-package eyebrowse
  :straight t
  :init
  (eyebrowse-mode)
  :bind (("C-c w c" . eyebrowse-create-window-config)
         ("C-c w q" . eyebrowse-close-window-config)
         ("C-c w n" . eyebrowse-next-window-config)
         ("C-c w p" . eyebrowse-prev-window-config)
         ("C-c w r" . eyebrowse-rename-window-config)
         ("C-c w 0" . eyebrowse-switch-to-window-config-0)
         ("C-c w 1" . eyebrowse-switch-to-window-config-1)
         ("C-c w 2" . eyebrowse-switch-to-window-config-2)
         ("C-c w 3" . eyebrowse-switch-to-window-config-3)
         ("C-c w 4" . eyebrowse-switch-to-window-config-4)
         ))

(use-package burly
  :straight t
  :bind(("C-c b w" . burly-bookmark-windows)
        ("C-c b l" . burly-open-last-bookmark)
        ("C-c b f" . burly-bookmark-frames)
        ("C-c b o" . burly-open-bookmark))
  :config
  (setq burly-frameset-filter-alist '((name . nil)
                                      (posframe-parent-buffer . :never)))
  (advice-add 'burly-bookmark-names :before (lambda () (bookmark-maybe-load-default-file))))

;; Enforce rules for popups
(use-package popper
  :straight t
  :defer 1
  :defines popper-echo-dispatch-actions
  :bind (("C-c b p" . popper-toggle-latest)
         :map popper-mode-map
         ("C-<tab>"   . popper-cycle))
  ;; :hook (emacs-startup . popper-mode)
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$" "\\*Pp Eval Output\\*$"
          "\\*Compile-Log\\*"
          "\\*Completions\\*"
          "\\*Warnings\\*"
          "\\*Async Shell Command\\*"
          "\\*Apropos\\*"
          "\\*Backtrace\\*"
          "\\*Calendar\\*"
          "\\*Embark Actions\\*"
          "\\*Finder\\*"
          "\\*Kill Ring\\*"
          "\\*Go-Translate\\*"
          bookmark-bmenu-mode
          comint-mode
          compilation-mode
          help-mode helpful-mode
          tabulated-list-mode
          Buffer-menu-mode

          flymake-diagnostics-buffer-mode
          flycheck-error-list-mode flycheck-verify-mode

          gnus-article-mode devdocs-mode
          grep-mode occur-mode rg-mode deadgrep-mode ag-mode pt-mode
          ivy-occur-mode ivy-occur-grep-mode
          youdao-dictionary-mode osx-dictionary-mode fanyi-mode

          "^\\*Process List\\*" process-menu-mode
          list-environment-mode cargo-process-mode

          "^\\*eshell.*\\*.*$"       eshell-mode
          "^\\*shell.*\\*.*$"        shell-mode
          "^\\*terminal.*\\*.*$"     term-mode
          "^\\*vterm[inal]*.*\\*.*$" vterm-mode

          "\\*DAP Templates\\*$" dap-server-log-mode
          "\\*ELP Profiling Restuls\\*" profiler-report-mode
          "\\*Paradox Report\\*$" "\\*package update results\\*$" "\\*Package-Lint\\*$"
          "\\*[Wo]*Man.*\\*$"
          "\\*ert\\*$" overseer-buffer-mode
          "\\*gud-debug\\*$"
          "\\*lsp-help\\*$" "\\*lsp session\\*$"
          "\\*quickrun\\*$"
          "\\*tldr\\*$"
          "\\*vc-.*\\*$"
          "^\\*elfeed-entry\\*$"
          "^\\*macro expansion\\**"

          "\\*Agenda Commands\\*" "\\*Org Select\\*" "\\*Capture\\*" "^CAPTURE-.*\\.org*"
          "\\*Gofmt Errors\\*$" "\\*Go Test\\*$" godoc-mode
          "\\*docker-.+\\*"
          "\\*prolog\\*" inferior-python-mode inf-ruby-mode swift-repl-mode
          "\\*rustfmt\\*$" rustic-compilation-mode rustic-cargo-clippy-mode
          rustic-cargo-outdated-mode rustic-cargo-run-mode rustic-cargo-test-mode))

  (setq popper-echo-dispatch-actions t)
  :config
  (popper-echo-mode 1)

  (with-no-warnings
    (defun my-popper-fit-window-height (win)
      "Determine the height of popup window WIN by fitting it to the buffer's content."
      (fit-window-to-buffer
       win
       (floor (frame-height) 3)
       (floor (frame-height) 3)))
    (setq popper-window-height #'my-popper-fit-window-height)

    (defun popper-close-window-hack (&rest _)
      "Close popper window via `C-g'."
      ;; `C-g' can deactivate region
      (when (and (called-interactively-p 'interactive)
                 (not (region-active-p))
                 popper-open-popup-alist)
        (let ((window (caar popper-open-popup-alist)))
          (when (window-live-p window)
            (delete-window window)))))
    (advice-add #'keyboard-quit :before #'popper-close-window-hack)))

(use-package treemacs
  :straight t
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-hide-dot-git-directory          t
          treemacs-no-png-images                   t
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" prelude-local-dir))
    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("C-c t 1" . treemacs-delete-other-windows)
        ("C-c t s" . treemacs-select-window)
        ("C-c t t" . treemacs)
        ("C-c t f" . treemacs-find-file)
        ("C-c t i" . treemacs-find-tag)
        ("C-c t d" . treemacs-select-directory)))

(use-package ace-window
  :straight t
  :bind (
         ("C-c w g" . ace-window)
         ))

(provide 'init-manage)
