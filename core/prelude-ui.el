(add-recipe-items
 '((:name spacemacs-theme :type github :pkgname "nashamri/spacemacs-theme"))
 )
(setq ui-require-packages
      (append
       '(spacemacs-theme)
       (mapcar 'el-get-as-symbol (mapcar 'el-get-source-name el-get-sources))))
(el-get 'sync ui-require-packages)

;; set theme to spacemacs-theme
(use-package spacemacs-common
  :demand nil
  :config
  (defun my/load-theme (appearance)
    "Load theme, taking current system APPEARANCE into consideration."
    (mapc #'disable-theme custom-enabled-themes)
    (pcase appearance
      ('light (load-theme 'spacemacs-light t))
      ('dark (load-theme 'spacemacs-dark t))))
  (add-hook 'ns-system-appearance-change-functions #'my/load-theme))
  
(set-frame-width (selected-frame) 120)
(set-frame-height (selected-frame) 39)
(set-face-attribute 'default nil :font "JetBrainsMono NF 15")

(setq inhibit-startup-screen t)
(fset 'yes-or-no-p 'y-or-n-p)
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))
(setq ring-bell-function 'ignore)
(blink-cursor-mode -1)

(provide 'prelude-ui)
