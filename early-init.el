(setq gc-cons-threshold most-positive-fixnum)

(setq native-comp-deferred-compilation nil)

(setq package-enable-at-startup nil)

(setq load-prefer-newer noninteractive)

(setq frame-inhibit-implied-resize t)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist))

(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'super)

;; set theme to spacemacs-theme
(add-to-list 'load-path (expand-file-name "core/spacemacs-theme" user-emacs-directory))
(require 'spacemacs-common)
(defun my/load-theme (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (load-theme 'spacemacs-light t))
    ('dark (load-theme 'spacemacs-dark t))))
(add-hook 'ns-system-appearance-change-functions #'my/load-theme)
