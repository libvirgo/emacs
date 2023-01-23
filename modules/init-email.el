;;; -*- lexical-binding: t; -*-

(use-package wanderlust
  :init
  (setq wl-from "libvirgo <sakurapetgirl@live.com>")
  (setq wl-message-ignored-field-list
		'("Received-SPF:"
		  ".*Received:"
		  ".*Path:"
		  ".*Id:"
		  "^References:"
		  "^Replied:"
		  "^Errors-To:"
		  "^Lines:"
		  "^Sender:"
		  ".*Host:"
		  "^Xref:"
		  "^Content-Type:"
		  "^Precedence:"
		  "^Status:"
		  "^X-VM-.*:"
		  "^ARC-.*:"
		  "^Authentication-Results:"
		  "^X-NAI-Spam.*:"
		  "X-.*:"
		  "MIME-Version:"
		  "Content-Transfer-Encoding:"
		  "DKIM-Signature:"
		  "List-.*:")
		shr-use-colors nil
		shr-use-fonts nil		  
		)
  :custom
  (wl-init-file (expand-file-name "wl" clytie-email-dir))
  (wl-alias-file (expand-file-name "im/Aliases" clytie-email-dir))
  (wl-score-files-directory (expand-file-name "elmo" clytie-email-dir))
  (wl-address-file (expand-file-name "addresses" clytie-email-dir))
  (wl-folders-file (expand-file-name "folders" clytie-email-dir))
  (wl-temporary-file-directory (expand-file-name "tmp" clytie-email-dir))
  (elmo-cache-directory (expand-file-name "elmo/cache" clytie-email-dir))
  (elmo-msgdb-directory (expand-file-name "elmo" clytie-email-dir))
  (elmo-localdir-folder-path (expand-file-name "mail" clytie-email-dir))
  :config
  (setq mime-view-text/html-previewer shr))
  
(provide 'init-email)
