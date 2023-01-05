;;; -*- lexical-binding: t; -*-

(use-package magit
  :bind (("C-c m m" . magit-file-dispatch))
  :config
  (setq magit-refresh-status-buffer nil)
    ;; (transient-append-suffix 'magit-submodule "-r"
    ;; '("-d" "shallow-clone update or add with --depth 1" "--depth 1"))
  (transient-append-suffix 'magit-commit "-e"
    '("-N" "Don't edit message" "--no-edit"))
  
  ;; git config -f .gitmodules submodule.<name>.shallow true
  ;; (defun magit-submodule-config (&optional args)
  ;; "git config for submodule."
  ;; (interactive (if current-prefix-arg
  ;;                  (list (cons "--amend" (magit-commit-arguments)))
  ;;                (list (magit-commit-arguments))))
  ;; (when (member "--all" args)
  ;;   (setq this-command 'magit-commit--all))
  ;;   (let ((default-directory (magit-toplevel)))
  ;;     (magit-run-git-with-editor "config" args)))
  ;; (transient-define-prefix 'my/magit-config ()
  ;; "Create a new commit or replace an existing commit."
  ;; :info-manual "(magit)Git config"
  ;; :man-page "git-config"
  ;; ["Arguments"
  ;;  ("-g" "Stage all modified and deleted files"   ("-g" "--global"))
  ;;  ]
  ;; [["Submodule"
  ;;   ("s" "submodule"         magit-submodule-config)]]
  ;; (interactive)
  ;; (if-let ((buffer (magit-commit-message-buffer)))
  ;;     (switch-to-buffer buffer)
  ;;   (transient-setup 'my/magit-config)))
  )

(use-package vc-mode
  :bind (("C-c v d" . vc-version-diff)))

(use-package project
  :bind (("C-c p p" . project-switch-project)
         ("C-c p f" . project-find-file)
         ("C-c p g" . project-find-regexp)
         ("C-c p b" . consult-project-buffer)
         ("C-c p m" . magit-project-status))
  :init
  (setq project-list-file (expand-file-name "projects" clytie-local-dir))
  :config
  (setq project-root-markers     '("Cargo.toml" "go.mod" "package.json" ".git"))
  (defun project-root-p (path)
    "Check if the current PATH has any of the project root markers."
    (catch 'found
      (dolist (marker project-root-markers)
        (when (file-exists-p (concat path marker))
          (throw 'found marker)))))
  (defun project-find-root (path)
    "Search up the PATH for `project-root-markers'."
    (let ((path (expand-file-name path)))
      (catch 'found
        (while (not (equal "/" path))
          (if (not (project-root-p path))
              (setq path (file-name-directory (directory-file-name path)))
            (throw 'found (cons 'transient path)))))))
  (add-to-list 'project-find-functions #'project-find-root)
  (defun my/project-files-in-directory (dir)
    "Use `fd' to list files in DIR."
    (let* ((default-directory dir)
           (localdir (file-local-name (expand-file-name dir)))
           (command (format "fd -H -t f -0 -E .git . %s" localdir)))
      (project--remote-file-names
       (sort (split-string (shell-command-to-string command) "\0" t)
             #'string<))))

  (cl-defmethod project-files ((project (head transient)) &optional dirs)
    "Override `project-files' to use `fd' in local projects."
    (mapcan #'my/project-files-in-directory
            (or dirs (list (project-root project)))))
  (setq magit-bind-magit-project-status nil)
  (define-key project-prefix-map "m" #'magit-project-status)
  (setq project-switch-commands
        '((project-find-file "Find file")
          (project-find-regexp "Find ripgrep")
          (project-find-dir "Find directory")
          (magit-project-status "Magit")))
  )

(use-package diff-hl
  :custom-face
  (diff-hl-change ((t (:inherit diff-changed :foreground unspecified :background unspecified))))
  (diff-hl-insert ((t (:inherit diff-added :background unspecified))))
  (diff-hl-delete ((t (:inherit diff-removed :background unspecified))))
  :bind (("C-c d s" . diff-hl-show-hunk)
         :repeat-map diff-hl-inline-popup-transient-mode-map
         ("C-f" . diff-hl-show-hunk-next)
         ("C-b" . diff-hl-show-hunk-previous)
         ("C-r" . diff-hl-show-hunk-revert-hunk)
         ("C-p" . diff-hl-inline-popup--popup-up)
         ("C-n" . diff-hl-inline-popup--popup-down)
         ("C-c" . diff-hl-show-hunk-copy-original-text)
         :map diff-hl-command-map
         ("SPC" . diff-hl-mark-hunk))
  :hook ((dired-mode . diff-hl-dired-mode))
  :init
  (setq diff-hl-draw-borders nil)
  (global-diff-hl-mode)
  :config
  ;; Highlight on-the-fly
  (diff-hl-flydiff-mode 1)
  ;; Set fringe style
  (setq-default fringes-outside-margins t)

  (with-no-warnings
    (defun my-diff-hl-fringe-bmp-function (_type _pos)
      "Fringe bitmap function for use as `diff-hl-fringe-bmp-function'."
      (define-fringe-bitmap 'my-diff-hl-bmp
        (vector (if sys/linuxp #b11111100 #b11100000))
        1 8
        '(center t)))
    (setq diff-hl-fringe-bmp-function #'my-diff-hl-fringe-bmp-function))
    ;; performance slow
    (with-eval-after-load 'magit
      (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
      (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)))

(use-package vc
  :bind (("C-c v h" . vc-region-history))
  )

(use-package eyebrowse
  :init
  (eyebrowse-mode)
  :bind (("C-c w q" . eyebrowse-close-window-config)
         ("C-c w r" . eyebrowse-rename-window-config)
         ))

;; Enforce rules for popups
(use-package popper
  :defer 3
  :defines popper-echo-dispatch-actions
  :bind (("C-c b p" . popper-toggle-latest)
         :map popper-mode-map
         ("C-<tab>"   . popper-cycle))
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
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-hide-dot-git-directory          t
          treemacs-no-png-images                   t
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" clytie-local-dir))
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

(provide 'init-project)
