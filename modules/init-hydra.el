;;; -*- lexical-binding: t; -*-

(use-package hydra)

(use-package major-mode-hydra
  :defer t
  :init
  (pretty-hydra-define fast-switch
	(:title "Switch" :color pink :quit-key ("q" "C-g"))
	("Window"
	 (("*" maximize-window)
	  ("w" ace-delete-other-windows :color blue)
	  ("s" ace-swap-window :color blue)
	  ("<" shrink-window-horizontally)
	  (">" enlarge-window-horizontally)
	  ("k" shrink-window)
	  ("j" enlarge-window))
	 "Search"
	 (("b" consult-buffer "search buffer" :color blue)
	  ("a" project-find-regexp "search project" :color blue)
	  ("i" consult-imenu :color blue)
	  )
	 "Project"
	 (("p" project-switch-project "switch project")
	  ("c" eyebrowse-create-window-config "create project window")
	  ("d" eyebrowse-close-window-config "close project window")
	  ("r" eyebrowse-rename-window-config "rename project window")
	  ("l" eyebrowse-next-window-config "switch next window")
	  ("h" eyebrowse-prev-window-config "switch prev window"))
	 "Misc"
	 (("+" org-capture :color blue)
	  ("=" org-agenda :color blue)
	  )))
  (pretty-hydra-define git-diff-hunk
	(:title "Git" :color pink :quit-key ("q" "C-g"))
	("Diff"
	 (("s" diff-hl-show-hunk :color blue)
	  ("n" diff-hl-show-hunk-next)
	  ("p" diff-hl-show-hunk-previous)
	  ("r" diff-hl-show-hunk-revert-hunk))
	 "Magit"
	 (("f" magit-file-dispatch :color blue)
	  ("b" magit-blame :color blue))))
  (require 'major-mode-hydra)
  :bind
  (("s-." . fast-switch/body)
   ("s-d" . git-diff-hunk/body)
   ("C-." . major-mode-hydra)))

(provide 'init-hydra)
