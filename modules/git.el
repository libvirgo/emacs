(setq el-get-sources
      '((:name diff-hl :type github :pkgname "dgutov/diff-hl")
        ))
(setq git-require-packages
      (append
       '(diff-hl
         )
       (mapcar 'el-get-as-symbol (mapcar 'el-get-source-name el-get-sources))))
(el-get 'sync git-require-packages)

(use-package diff-hl
  :bind (:map diff-hl-command-map
              ("SPC" . diff-hl-mark-hunk))
  :hook ((after-init . global-diff-hl-mode)
         (after-init . global-diff-hl-show-hunk-mouse-mode)
         (dired-mode . diff-hl-dired-mode))
  :init (setq diff-hl-draw-borders nil)
  :config
  ;; Highlight on-the-fly
  (diff-hl-flydiff-mode 1)

  ;; Set fringe style
  (setq-default fringes-outside-margins t)

  (with-eval-after-load 'magit
    (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)))

(provide 'git)
