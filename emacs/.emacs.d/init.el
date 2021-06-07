;;; init.el : where all the magic happens

(defvar lg-configuration-path (expand-file-name "lisp" "~/.emacs.d"))
(add-to-list 'load-path lg-configuration-path)

(require 'core) ;; should be loaded first
(require 'lg-keybindings) (message "loaded keybindings")
(require 'lg-window) (message "loaded window")
(require 'lg-completion) (message "loaded completion")
(require 'lg-ui) (message "loaded ui")
(require 'lg-tools) (message "loaded tools") 
(require 'lg-exwm) (message "loaded exwm")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("076ee9f2c64746aac7994b697eb7dbde23ac22988d41ef31b714fc6478fee224" "2b502f6e3bf0cba42fe7bf83a000f2d358a7020a7780a8682fcfed0c9dbffb5f" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
