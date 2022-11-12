;;; lg-fun --- Fun stuff in emacs -*- lexical-binding: t; -*-
	
(use-package cowsay
  :straight t
  :commands cowsay-string
  :config
  (cowsay-load-cows-directory "~/.emacs.d/straight/repos/emacs-cowsay/cows"))

;; display engine hacks
(use-package magic-buffer
  :commands (magic-buffer)
  :straight (magic-buffer :host github :repo "sabof/magic-buffer"))

;; typing test
(use-package typit
  :straight t
  :commands (typit-test typit-basic-test typit-advanced-test)
  :custom (typit-dict "french.txt"))

;; watch anime
(use-package enime
  :straight (:host github :repo "xl666/enime" :files (:defaults "video_scrapping.sh"))
  :commands (enime-main-transient))

(use-package suggest :straight t :commands suggest)
(use-package fireplace :straight t)
(use-package snow :straight t :commands snow)
(use-package vimgolf :defer t :straight t)
(use-package dad-joke :straight t :defer t)
(use-package xkcd :straight t :defer t)
(use-package wordel :straight t :defer t)

(provide 'lg-fun)
