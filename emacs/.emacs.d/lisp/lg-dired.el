;;; lg-dired --- configuration for the directory editor -*- lexical-binding: t; -*-

;;; Dired
(use-package dired
  :hook
  (dired-mode . dired-hide-details-mode)
  :custom
  (dired-kill-when-opening-new-dired-buffer nil)
  (dired-clean-confirm-killing-deleted-buffers nil)
  (dired-listing-switches "-al --group-directories-first")
  (dired-dwim-target t)
  (dired-compress-directory-default-suffix ".zip")
  :general
  (:keymaps 'dired-mode-map
	    "<mouse-8>" #'dired-up-directory)
  (:keymaps 'dired-mode-map
	    :states 'normal
	    "h" #'dired-up-directory
	    "l" #'dired-find-file))

;;; dired-async-mode
(use-package dired-async
  :hook (dired-mode . dired-async-mode))

;;; Diredfl
(use-package diredfl
  :straight t
  :after dired
  :hook (dired-mode . diredfl-mode))

;;; Dired-sidebar
(use-package dired-sidebar
  :straight t
  :commands (dired-sidebar-find-file dired-sidebar-toggle)
  :bind (:map dired-sidebar-mode-map ("<localleader>m" . emms-play-dired))
  :general
  (general-def :keymaps 'dired-sidebar-mode-map :states 'normal
    "h" #'dired-sidebar-up-directory
    "l" #'dired-sidebar-find-file)
  :hook
  (dired-sidebar-mode . (lambda () (setq-local header-line-format "Dired-sidebar")))
  :custom
  (dired-sidebar-width 30)
  (dired-sidebar-no-delete-other-windows t))

;;; Dired-hide-dotfiles
(use-package dired-hide-dotfiles
  :after dired
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :straight t)

(use-package dired-git
  :disabled t
  :straight t
  :hook (dired-mode . dired-git-mode))

(provide 'lg-dired)
