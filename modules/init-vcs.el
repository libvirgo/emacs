;;; -*- lexical-binding: t; -*-

(use-package magit
  :straight (:host github :type git :repo "magit/magit" :depth full
				   :fork (:repo "libvirgo" :protocol ssh))
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
  :straight (:type built-in)
  :bind (("C-c v d" . vc-version-diff)))

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

(use-package vundo
  :bind (("C-c u" . 'vundo))
  :config
  (setq undohist-directory (expand-file-name "undohist" clytie-cache-dir))
  (undohist-initialize)
  (defun my/vundo-diff ()
    (interactive)
    (let* ((orig vundo--orig-buffer)
           (source (vundo--current-node vundo--prev-mod-list))
           (dest (vundo-m-parent source)))
      (if (or (not dest) (eq source dest))
          (message "vundo diff not available.")
	(let ((buf (make-temp-name (concat (buffer-name orig) "-vundo-diff"))))
          (vundo--move-to-node source dest orig vundo--prev-mod-list)
          (with-current-buffer (get-buffer-create buf)
	    (insert-buffer orig))
          (vundo--refresh-buffer orig (current-buffer) 'incremental)
          (vundo--move-to-node dest source orig vundo--prev-mod-list)
          (vundo--refresh-buffer orig (current-buffer) 'incremental)
          (diff-buffers buf orig)
          (kill-buffer buf)))))
  (keymap-set vundo-mode-map "d" #'my/vundo-diff))

(use-package smerge-mode
  :diminish
  :config
  (pretty-hydra-define smerge-mode-hydra
	(:title "Merge" :color pink :quit-key ("q" "C-g"))
	("Move"
     (("n" smerge-next "next")
      ("p" smerge-prev "previous"))
     "Keep"
     (("b" smerge-keep-base "base")
      ("u" smerge-keep-upper "upper")
      ("l" smerge-keep-lower "lower")
      ("a" smerge-keep-all "all")
      ("RET" smerge-keep-current "current")
      ("C-m" smerge-keep-current "current"))
     "Diff"
     (("<" smerge-diff-base-upper "upper/base")
      ("=" smerge-diff-upper-lower "upper/lower")
      (">" smerge-diff-base-lower "upper/lower")
      ("R" smerge-refine "refine")
      ("E" smerge-ediff "ediff"))
     "Other"
     (("C" smerge-combine-with-next "combine")
      ("r" smerge-resolve "resolve")
      ("k" smerge-kill-current "kill")
      ("ZZ" (lambda ()
              (interactive)
              (save-buffer)
              (bury-buffer))
	   "Save and bury buffer" :exit t))))
  (keymap-set smerge-mode-map "C-c m s" #'smerge-mode-hydra/body)
  :hook ((find-file . (lambda ()
                        (save-excursion
						  (goto-char (point-min))
						  (when (re-search-forward "^<<<<<<< " nil t)
                            (smerge-mode 1)))))
         (magit-diff-visit-file . (lambda ()
                                    (when smerge-mode
									  (smerge-mode-hydra/body))))
		 )
  )


(provide 'init-vcs)
