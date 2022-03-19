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
  :commands (typit-test
	     typit-basic-test
	     typit-advanced-test)
  :custom (typit-dict "french.txt"))

;;; wordel
(use-package wordel
  :straight (:host github :repo "progfolio/wordel" :files (:defaults "words.txt"))
  :defer t)

;;; enime : watch anime
(use-package esxml
  :straight t)
(use-package enime
  :straight (:host github :repo "xl666/enime" :files (:defaults "video_scrapping.sh"))
  :commands (enime-main-transient))

;;; vimgolf
(use-package vimgolf
  :defer t
  :straight t)

;;; dad jokes
(use-package dad-joke
  :straight t
  :defer t)

(provide 'lg-fun)
