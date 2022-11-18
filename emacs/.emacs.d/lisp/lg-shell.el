;;; lg-shell --- configuration for multiple shells -*- lexical-binding: t; -*-
;; Author: Lucas Gruss
;; This file is NOT part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:

(use-package vterm
  :straight (vterm :type git :repo "akermu/emacs-libvterm")
  :ensure-system-package (libvterm . libvterm-dev)
  :commands vterm
  :custom
  (vterm-shell "bash")
  (vterm-always-compile-module t)
  (vterm-module-cmake-args "-DUSE_SYSTEM_VTERM=YES"))

(use-package eshell
  :defer t
  :init
  (defun emacs-run-eshell ()
    (interactive)
    (if (frame-parameter nil 'kill-me)
	(delete-frame)
      (let ((frame (condition-case nil
		       (select-frame-by-name "eshell/terminal")
		     (error nil))))
	(if frame
	    (select-frame-by-name "eshell/terminal")
	  (with-selected-frame (make-frame '((name . "eshell/terminal")
					     (minibuffer . nil)
					     (undecorated . t)
					     (width . 0.7)
					     (height . 0.4)
					     (top . 0.5)
					     (display . ":0")
					     (left . 0.5)
					     (kill-me . t)))
	    (unwind-protect (eshell))))))))

(use-package eshell-toggle
  :straight t
  :custom
  (eshell-toggle-window-side 'right)
  (eshell-toggle-size-fraction 2)
  :bind ("s-<return>" . eshell-toggle))

(use-package eshell-info-banner
  :commands eshell
  :straight t
  :hook (eshell-banner-load . eshell-info-banner-update-banner))

(use-package eshell-vterm
  :straight t
  :after eshell vterm
  :config (eshell-vterm-mode +1))

(use-package eshell-git-prompt
  :straight t
  :after eshell
  :config (eshell-git-prompt-use-theme 'powerline))

(use-package dtache
  :disabled t
  :straight (:host gitlab :repo "niklaseklund/dtache")
  :ensure-system-package dtach
  :hook (after-init . dtache-setup)
  :bind (([remap async-shell-command] . dtache-shell-command)
	 :map dtache-shell-mode-map
	 ("C-c C-q" . dtache-detach-dwim))) 

(use-package dtache-eshell
  :after dtache
  :hook (after-init . dtache-eshell-setup)
  :bind (:map dtache-eshell-mode-map
	      (("<S-return>" . dtache-eshell-send-input)
	       ("<C-return>" . dtache-eshell-attach)
	       ("C-c C-q" . dtache-detach-dwim)))) 

(use-package dtache-consult
  :after dtache
  :bind ([remap dtache-open-session] . dtache-consult-session))

(provide 'lg-shell)
;;; lg-shell.el ends here
