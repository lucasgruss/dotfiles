;;; lg-erc: configuration for the emacs irc client
;; Author: Lucas Gruss
;; This file is NOT part of GNU Emacs.
;;
;;; Commentary:
;; Inspiration from:
;; https://github.com/SystemCrafters/systemcrafters.github.io/blob/6e0686022b84c291686fc773d2974e7be02ec2b9/content/live-streams/june-04-2021.org
;;
;;; Code:


;; /msg NickServ info 
;; /msg NickServ register password email-address
;; /msg NickServ VERIFY REGISTER nickname secret-password
;; /msg nickserv identify password
;; /join #emacs

;;; erc
(use-package erc
  :commands (erc erc-tls)
  :init
  (defalias 'erc 'erc-tls)
  (defun lg/connect-irc ()
    (interactive)
    (erc-tls
     :server "irc.libera.chat"
     :port 6697
     :nick "poinkalu"
     ;; This is using password-store.el.  Not needed if you use auth-source!
     :password (password-store-get "libera-chat")))

  :custom
  (erc-prompt-for-password t)
  (erc-server "irc.libera.chat")
  (erc-nick "poinkalu")
  (erc-user-full-name "Lucas Gruss")
  (erc-track-shorten-start 8)
  ;; (erc-autojoin-channels-alist '(("irc.libera.chat" "#systemcrafters" "#emacs")))
  (erc-kill-buffer-on-part t)
  (erc-auto-query 'bury))

;;; erc-images
(use-package erc-image
  :straight t
  :after erc
  :custom
  (erc-image-inline-rescale 300)
  :config
  (add-to-list 'erc-modules 'image))

(provide 'lg-erc)
;;; lg-erc.el ends here
