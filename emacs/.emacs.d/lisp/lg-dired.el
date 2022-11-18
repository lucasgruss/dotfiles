;;; lg-dired --- configuration for the directory editor -*- lexical-binding: t; -*-
;; Author: Lucas Gruss
;; This file is NOT part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:

(use-package dired
  :hook (dired-mode . dired-hide-details-mode)
        (dired-mode . auto-revert-mode)
  :custom
  (dired-kill-when-opening-new-dired-buffer nil)
  (dired-clean-confirm-killing-deleted-buffers nil)
  (dired-listing-switches "-al --group-directories-first")
  (dired-dwim-target t)
  (dired-compress-directory-default-suffix ".zip")
  (dired-mouse-drag-files t)
  :general
  (:keymaps 'dired-mode-map
	    "<mouse-8>" #'dired-up-directory)
  (:keymaps 'dired-mode-map
	    :states 'normal
	    "h" #'dired-up-directory
	    "l" #'dired-find-file))

(use-package dired-x 
  :after dired
  :custom (dired-omit-extensions "config"))

;; make dired operations asynchronous so they don't block the UI
(use-package dired-async
  :after dired
  :hook (dired-mode . dired-async-mode))

(use-package diredfl
  :disabled t ;; apparently, it does not play well with all-the-icons-dired
  :straight t
  :after dired
  :hook (dired-mode . diredfl-mode))

(use-package dired-sidebar
  :straight t
  :bind (:map dired-sidebar-mode-map ("<localleader>m" . emms-play-dired))
  :general
  (:keymaps 'dired-sidebar-mode-map
	    :states 'normal
	    "h" #'dired-sidebar-up-directory
	    "l" #'dired-sidebar-find-file)
  :hook (dired-sidebar-mode . (lambda () (setq-local header-line-format "Dired-sidebar")))
  :custom (dired-sidebar-width 30)
  (dired-sidebar-no-delete-other-windows t))

(use-package dired-hide-dotfiles
  :after dired
  :straight t)

(provide 'lg-dired)
;;; lg-dired.el ends here
