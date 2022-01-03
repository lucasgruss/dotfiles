;;; lg-shell : configuration for multiple shells -*- lexical-binding: t; -*-  

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
(use-package eshell-info-banner
  :commands eshell
  :straight (eshell-info-banner :type git
                                :host github
                                :repo "phundrak/eshell-info-banner.el")
  :hook (eshell-banner-load . eshell-info-banner-update-banner))

;;; eshell-vterm
(use-package eshell-vterm
  :straight t
  :after eshell
  :config
  (eshell-vterm-mode +1))

(provide 'lg-shell)
