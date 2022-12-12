;;; -*- lexical-binding: t; -*-

(use-package diff-hl
  :straight t
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

(provide 'init-vc)
