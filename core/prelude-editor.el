(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq default-enable-multibyte-characters nil)

(el-get 'sync 'hungry-delete)

(use-package hungry-delete
  :init (global-hungry-delete-mode)
  )

(provide 'prelude-editor)
