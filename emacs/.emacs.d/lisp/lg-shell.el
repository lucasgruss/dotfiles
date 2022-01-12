;;; lg-shell --- configuration for multiple shells -*- lexical-binding: t; -*-

;;; Vterm
(use-package vterm
  ;; :ensure-system-package (libvterm . libvterm-dev)
  :straight (vterm :type git :repo "akermu/emacs-libvterm")
  :commands vterm
  :custom
  (vterm-shell "bash")
  (vterm-always-compile-module t)
  (vterm-module-cmake-args "-DUSE_SYSTEM_VTERM=YES"))

;;; Multi-vterm
(use-package multi-vterm
  :straight t
  :after vterm)

;;; Vterm-toggle
;; run-or-raise-or-dismiss for vterm
(use-package vterm-toggle
  :straight t
  :bind ("s-<return>" . 'lg/vterm-toggle)
  :commands (vterm-toggle)
  :custom
  (vterm-toggle-fullscreen-p nil)
  :init
  (defun lg/vterm-toggle ()
    (interactive)
    (if (string= (frame-parameter nil 'name) "yequake-vterm")
	(delete-frame (select-frame-by-name "yequake-vterm"))
      (vterm-toggle))))

;;; Eshell
;;;; eshell-toggle 
(use-package eshell-toggle
  :straight t
  :custom
  (eshell-toggle-window-side 'right)
  (eshell-toggle-size-fraction 2)
  :bind ("s-<return>" . eshell-toggle))

;;;; eshell-info-banner
(use-package eshell-info-banner
  :commands eshell
  :straight (eshell-info-banner :type git
                                :host github
                                :repo "phundrak/eshell-info-banner.el")
  :hook (eshell-banner-load . eshell-info-banner-update-banner))

;;;; eshell-vterm
(use-package eshell-vterm
  :straight t
  :after eshell
  :config (eshell-vterm-mode +1))

;;;; eshell-git-prompt
(use-package eshell-git-prompt
  :straight t
  :after eshell
  :config
  (eshell-git-prompt-use-theme 'powerline))

;;; Dtache
(use-package dtache
  :straight (:host gitlab :repo "niklaseklund/dtache")
  :ensure-system-package dtach
  :hook (after-init . dtache-setup)
  :bind (([remap async-shell-command] . dtache-shell-command)
	 :map dtache-shell-mode-map
	 ("C-c C-q" . dtache-detach-dwim))) 

;;;; dtach-eshell
(use-package dtache-eshell
  :after dtache
  :hook (after-init . dtache-eshell-setup)
  :bind (:map dtache-eshell-mode-map
	      (("<S-return>" . dtache-eshell-send-input)
	       ("<C-return>" . dtache-eshell-attach)
	       ("C-c C-q" . dtache-detach-dwim)))) 

;;;; dtach-consult
(use-package dtache-consult
  :after dtache
  :bind ([remap dtache-open-session] . dtache-consult-session))

(provide 'lg-shell)
