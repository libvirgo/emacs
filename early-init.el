;;; -*- lexical-binding: t; -*-

(when (boundp 'native-comp-eln-load-path)
  (when (eq system-type 'darwin)
    (setq native-comp-driver-options '("-Wl,-w")))
  ;; Don't store eln files in ~/.emacs.d/eln-cache (where they can easily be
  ;; deleted by 'doom upgrade').
  ;; REVIEW Use `startup-redirect-eln-cache' when 28 support is dropped

  (setq native-comp-cache-dir (expand-file-name (expand-file-name ".local/eln/" user-emacs-directory) comp-native-version-dir))
  (setcar native-comp-eln-load-path native-comp-cache-dir)
  (startup-redirect-eln-cache native-comp-cache-dir)
  (setq native-compile-target-directory native-comp-cache-dir)
  
  ;; UX: Suppress compiler warnings and don't inundate users with their popups.
  ;;   They are rarely more than warnings, so are safe to ignore.
  (setq native-comp-async-report-warnings-errors init-file-debug
        native-comp-warning-on-missing-source init-file-debug)

  ;; UX: By default, native-comp uses 100% of half your cores. If you're
  ;;   expecting this this should be no issue, but the sudden (and silent) spike
  ;;   of CPU and memory utilization can alarm folks, overheat laptops, or
  ;;   overwhelm less performant systems.
  (define-advice comp-effective-async-max-jobs (:before (&rest _) set-default-cpus)
    "Default to 1/4 of cores in interactive sessions and all of them otherwise."
    (and (null comp-num-cpus)
         (zerop native-comp-async-jobs-number)
         (setq comp-num-cpus
               (max 1 (/ (num-processors) (if noninteractive 1 4)))))))


(setq native-comp-deferred-compilation t)

(setq doom-gc-cons-threshold 167772160)
(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
  (lambda ()
    (setq gc-cons-threshold doom-gc-cons-threshold ; 160mb
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



(setq package-enable-at-startup nil ; don't auto-initialize!
      ;; don't add that `custom-set-variables' block to my init.el!
      package--init-file-ensured t)

(setq load-prefer-newer noninteractive)

(setq frame-inhibit-implied-resize t)

(load (expand-file-name "core/core" user-emacs-directory))
