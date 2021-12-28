;; -*- lexical-binding: t; -*-
;;; lg-fun --- Fun stuff in emacs 
	
;;; Cowsay
(use-package cowsay
  :straight t
  :config
  (cowsay-load-cows-directory "~/.emacs.d/straight/repos/emacs-cowsay/cows"))

;;; Suggest
(use-package suggest
  :straight t
  :commands suggest)

;;; Magic buffer
(use-package magic-buffer
  :commands (magic-buffer)
  :straight (magic-buffer :host github :repo "sabof/magic-buffer"))

;;; dank-mode (reddit)
(use-package dank-mode
  :straight (dank-mode :host github :repo "john2x/dank-mode")
  :commands (dank-mode)
  :config
  (use-package markdown-mode
    :straight t))

;;; fireplace
(use-package fireplace
  :straight t)
  
;;; snow
(use-package snow
  :straight t
  :commands snow)

;;; bonjour madame
(use-package bonjourmadame
  :straight t
  :config
  (setq bonjourmadame--regexp
	(rx
	 "<img" (1+ space)
	 "src=\"" (group "https://" (1+ nonl) "tumblr.com" (1+ nonl) "." (or "png" "jpg" "jpeg" "gif")) "\""
	 (1+ space)
	 "alt=\"" (group (0+ (not (any "\"")))) "\"")))

(provide 'lg-fun)
