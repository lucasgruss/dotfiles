;;; lg-fun --- Fun stuff in emacs -*- lexical-binding: t; -*-
	
;;; Cowsay
(use-package cowsay
  :straight t
  :commands cowsay-string
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

;;; typit
(use-package typit
  :straight t
  :custom (typit-dict "french.txt"))

(provide 'lg-fun)
