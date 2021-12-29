;;; lg-erc: configuration for the emacs irc client
;; Inspiration from:
;; https://github.com/SystemCrafters/systemcrafters.github.io/blob/6e0686022b84c291686fc773d2974e7be02ec2b9/content/live-streams/june-04-2021.org

;;; erc
(use-package erc
  :commands (erc erc-tls)
  :init
  (defalias 'erc 'erc-tls)
  :custom
  (erc-server "irc.libera.chat"
	      erc-nick "poinkalu"   
	      erc-user-full-name "Lucas Gruss"
	      erc-track-shorten-start 8
	      ;; erc-autojoin-channels-alist '(("irc.libera.chat" "#systemcrafters" "#emacs"))
	      erc-kill-buffer-on-part t
	      erc-auto-query 'bury))

;;; erc-images
(use-package erc-image
  :straight t
  :after erc
  :custom
  (erc-image-inline-rescale 300)
  :config
  (add-to-list 'erc-modules 'image))

(provide 'lg-erc)
