;;; -*- lexical-binding: t; -*-

(use-package org-modern
  :hook
  (org-mode . org-modern-mode)
  (org-agenda-finalize . org-modern-agenda)
  :init
  (setq org-todo-keywords '((sequence "TODO(t)" "DOING(i/!)" "|" "DONE(d@/!)" "ABORT(a)"))
		org-enforce-todo-dependencies t
		org-checkbox-hierarchical-statistics nil
		;; Edit settings
		org-auto-align-tags nil
		org-tags-column 0
		org-catch-invisible-edits 'show-and-error
		org-special-ctrl-a/e t
		org-insert-heading-respect-content t

		;; Org styling, hide markup etc.
		org-hide-emphasis-markers t
		org-pretty-entities t
		org-ellipsis "…"
		;; Agenda styling
		org-agenda-tags-column 0
		org-agenda-block-separator ?─
		org-agenda-time-grid
		'((daily today require-timed)
		  (800 1000 1200 1400 1600 1800 2000)
		  " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
		org-agenda-restore-windows-after-quit t
		org-agenda-compact-blocks t
		
		org-agenda-current-time-string
		"⭠ now ─────────────────────────────────────────────────")
  (add-to-list 'electric-pair-inhibit-predicate-mode-chars-alist
               '(org-mode . (?\<)))
  )

(use-package org
  :hook (org-mode . (lambda () (setq truncate-lines nil)))
  :init
  ;; agenda and capture
  (setq clytie-agenda-dir (expand-file-name "Documents/org/agenda" (getenv "HOME")))
  (setq org-capture-templates nil
		org-agenda-files (list clytie-agenda-dir))
  (defun capture-todo-week-file (p)
	(let ((file-name (expand-file-name (format "%s-todo.org" (format-time-string "%Y-%m_%W")) p)))
	  (unless (file-exists-p file-name)
		(make-empty-file file-name)
		(append-to-file "#+title:Task\n#+author:libvirgo\n\n* Work\n\n* Daily" nil file-name))
	  file-name))
  (add-to-list 'org-capture-templates '("t" "Tasks"))
  (add-to-list 'org-capture-templates
			   `("tw" "Work Task" entry
				 (file+olp ,(capture-todo-week-file clytie-agenda-dir) "Work")
				 "* TODO %^{task name} %^t->%^t %^G"))
  (add-to-list 'org-capture-templates
			   `("td" "Daily Task" entry
				 (file+olp ,(capture-todo-week-file clytie-agenda-dir) "Daily")
				 "* TODO %^{task name} %^t->%^t %^G"))
  ;; save password by org-capture
  (require 'epa-file)
  (epa-file-enable)
  (setq epa-file-select-keys 0)
  (setq epa-pinentry-mode 'loopback)
  (defun random-alphanum ()
	(let* ((charset "abcdefghijklmnopqrstuvwxyz0123456789")
           (x (random 36)))
      (char-to-string (elt charset x))))
  (defun create-password ()
	(let ((value ""))
      (dotimes (number 16 value)
		(setq value (concat value (random-alphanum))))))
  (defun get-or-create-password ()
	(setq password (read-string "Password: "))
	(if (string= password "")
		(create-password)
      password))
  (add-to-list 'org-capture-templates
			   '("p" "Passwords" entry (file "~/Documents/org/passwords.org.gpg")
				 "* %U - %^{title} %^G\n\n  - Name: %^{Name}\n  - Password: %(get-or-create-password)"
				 :empty-lines 1 :kill-buffer t)))

(use-package org-autolist
  :hook (org-mode . org-autolist-mode))

(use-package org-roam
  :custom
  org-roam-directory (file-truename "~/Documents/org/notes")
  :bind (("C-c n l" . org-roam-buffer-toggle)
		 ("C-c n f" . org-roam-node-find)
		 ("C-c n g" . org-roam-graph)
		 ("C-c n i" . org-roam-node-insert)
		 ("C-c n c" . org-roam-capture)
		 ("C-c n j" . org-roam-dailies-capture-today))
  :init
  (setq org-roam-db-location (expand-file-name "org-roam.db" clytie-cache-dir))
  :config
  (org-roam-db-autosync-mode))

(use-package org-roam-ui
  :config
  (setq org-roam-ui-sync-theme t
		org-roam-ui-follow t
		org-roam-ui-update-on-save t
		org-roam-ui-open-on-start t))

(provide 'init-org)
