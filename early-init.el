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


(defconst sys/linuxp
  (and (eq system-type 'gnu/linux)
       (not (string-match "-[Mm]icrosoft" operating-system-release)))
  "Are we running on a GNU/Linux system?")
