;;; -*- lexical-binding: t; -*-

(require 'cl-lib)

(setq theme-directory (expand-file-name ".local/lib/spacemacs-theme" user-emacs-directory))
(add-to-list 'load-path theme-directory)
(require 'spacemacs-common)

(if (eq system-type 'darwin)
    (progn
      (defun my/load-theme (appearance)
        "Load theme, taking current system APPEARANCE into consideration."
        (mapc #'disable-theme custom-enabled-themes)
        (pcase appearance
          ('light (load-theme 'spacemacs-light t))
          ('dark (load-theme 'spacemacs-dark t))))
      (add-hook 'ns-system-appearance-change-functions #'my/load-theme)
      (setq mac-option-modifier 'meta)
      (setq mac-command-modifier 'super)
	  (push '(ns-transparent-titlebar . t) default-frame-alist))
  (progn
	(setq circadian-directory (expand-file-name ".local/lib/circadian" user-emacs-directory))
	(add-to-list 'load-path circadian-directory)
	(require 'circadian)
	(setq calendar-longitude 120.2)
	(setq calendar-latitude 30.2)
	(setq circadian-themes '((:sunrise . spacemacs-light)
							 (:sunset . spacemacs-dark)))
	(circadian-setup)))

(defun spacemacs/reset-frame-size (&optional frame)
    (interactive)
    (when frame
      (select-frame frame))
    (set-frame-width (selected-frame) 120)
    (set-frame-height (selected-frame) 39))
(add-to-list 'default-frame-alist '(height . 39))
(add-to-list 'default-frame-alist '(width . 120))
(when (eq system-type 'gnu/linux)
  (add-to-list 'default-frame-alist '(alpha-background . 90))
  )
(add-hook 'after-make-frame-functions 'spacemacs/reset-frame-size)

(defun +my/better-font()
  (interactive)
  ;; english font
  (if (display-graphic-p)
      (progn
        ;; (set-face-attribute 'default nil :font (format "%s:pixelsize=%d" "JetBrainsMono NF" 15))
		(cond
		 ((find-font (font-spec :name "Iosevka NF"))
		  (set-face-attribute 'default nil :font (format "%s:pixelsize=%d" "Iosevka NF" 22)))
		 ((find-font (font-spec :name "Iosevka Nerd Font"))
		  (set-face-attribute 'default nil :font (format "%s:pixelsize=%d" "Iosevka Nerd Font" 15))))
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
(setq use-short-answers t)

;; remove gui elements.
(progn
  (setq ns-use-proxy-icon nil)
  (setq frame-title-format
		'((:eval "")))
  (defun spacemacs//removes-gui-elements ()
	"Remove the menu bar, tool bar and scroll bars."
	;; removes the GUI elements
	(unless (eq window-system 'mac)
      (when (and (fboundp 'menu-bar-mode) (not (eq menu-bar-mode -1)))
		(menu-bar-mode -1)))
	(when (and (fboundp 'scroll-bar-mode) (not (eq scroll-bar-mode -1)))
      (scroll-bar-mode -1))
	(when (and (fboundp 'tool-bar-mode) (not (eq tool-bar-mode -1)))
      (tool-bar-mode -1))
	;; tooltips in echo-aera
	(when (and (fboundp 'tooltip-mode) (not (eq tooltip-mode -1)))
      (tooltip-mode -1)))
  (spacemacs//removes-gui-elements)
  (push '(vertical-scroll-bars) default-frame-alist)
  (pixel-scroll-precision-mode)
  (blink-cursor-mode -1))

(setq ring-bell-function 'ignore)

(setq-default mode-line-format nil)
