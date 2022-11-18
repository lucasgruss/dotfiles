;;; lg-fun --- Fun stuff in emacs -*- lexical-binding: t; -*-
;; Author: Lucas Gruss
;; This file is NOT part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:
	
(use-package cowsay
  :straight t
  :commands cowsay-string
  :config
  (cowsay-load-cows-directory "~/.emacs.d/straight/repos/emacs-cowsay/cows"))

(use-package magic-buffer ;; display engine hacks
  :commands (magic-buffer)
  :straight (magic-buffer :host github :repo "sabof/magic-buffer"))

(use-package typit
  :straight t
  :commands (typit-test typit-basic-test typit-advanced-test)
  :custom (typit-dict "french.txt"))

(use-package enime ;; watch anime
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
;;; lg-fun.el ends here
