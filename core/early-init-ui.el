(require 'cl-lib)
(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist))
(if (eq system-type 'darwin)
    (progn
      (setq theme-directory (expand-file-name "core/spacemacs-theme/" user-emacs-directory))
      (add-to-list 'load-path theme-directory)

      (require 'spacemacs-common)
      (defun my/load-theme (appearance)
        "Load theme, taking current system APPEARANCE into consideration."
        (mapc #'disable-theme custom-enabled-themes)
        (pcase appearance
          ('light (load-theme 'spacemacs-light t))
          ('dark (load-theme 'spacemacs-dark t))))
      (add-hook 'ns-system-appearance-change-functions #'my/load-theme)
      (setq mac-option-modifier 'meta)
      (setq mac-command-modifier 'super)
      )
  )

(defun spacemacs/reset-frame-size (&optional frame)
    (interactive)
    (when frame
      (select-frame frame))
    (set-frame-width (selected-frame) 120)
    (set-frame-height (selected-frame) 39))
(add-hook 'after-make-frame-functions 'spacemacs/reset-frame-size)

(defun +my/better-font()
  (interactive)
  ;; english font
  (if (display-graphic-p)
      (progn
        (set-face-attribute 'default nil :font (format "%s:pixelsize=%d" "JetBrainsMono NF" 15))
        ;; chinese font
        (dolist (charset '(kana han symbol cjk-misc bopomofo))
          (set-fontset-font (frame-parameter nil 'font)
                            charset
                            (font-spec :family "Sarasa Mono SC"))))
    ))
(defun +my|init-font(frame)
  (with-selected-frame frame
    (if (display-graphic-p)
        (+my/better-font))))
(add-hook 'after-make-frame-functions #'+my|init-font)
(+my/better-font)

(setq inhibit-startup-screen t)
(fset 'yes-or-no-p 'y-or-n-p)
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))
(setq ring-bell-function 'ignore)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(blink-cursor-mode -1)


