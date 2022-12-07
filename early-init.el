(setq doom-gc-cons-threshold 16777216)
(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
  (lambda ()
    (setq gc-cons-threshold doom-gc-cons-threshold ; 16mb
          gc-cons-percentage 0.1)))

(defun doom-defer-garbage-collection-h ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun doom-restore-garbage-collection-h ()
  ;; Defer it so that commands launched immediately after will enjoy the
  ;; benefits.
  (run-at-time
   1 nil (lambda () (setq gc-cons-threshold doom-gc-cons-threshold))))
(add-hook 'minibuffer-setup-hook #'doom-defer-garbage-collection-h)
(add-hook 'minibuffer-exit-hook #'doom-restore-garbage-collection-h)

(defvar doom--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
;; Alternatively, restore it even later:
(add-hook 'emacs-startup-hook
  (lambda ()
    (setq file-name-handler-alist doom--file-name-handler-alist)))

(setq native-comp-deferred-compilation nil)

(setq package-enable-at-startup nil ; don't auto-initialize!
      ;; don't add that `custom-set-variables' block to my init.el!
      package--init-file-ensured t)

(setq load-prefer-newer noninteractive)

(setq frame-inhibit-implied-resize t)

(setq prelude-init-ui-file (expand-file-name "core/early-init-ui.el" user-emacs-directory))
(defcustom native-comp-driver-options (when (eq system-type 'darwin)
                                        '("-Wl,-w"))
  "darwin native comp"
  :type '(repeat string)
  :version "28.1")
(defcustom warning-suppress-types '((comp))
  "don't warn about native-comp"
  :type '(repeat (repeat symbol))
  :version "22.1")
(setq native-target-dir (expand-file-name (expand-file-name "local/eln/" user-emacs-directory) comp-native-version-dir))
(when (boundp 'native-comp-eln-load-path)
  (setq native-compile-target-directory native-target-dir)
  (startup-redirect-eln-cache native-target-dir))
(load prelude-init-ui-file)
