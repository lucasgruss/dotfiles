;;; lg-mail --- configuration for mail -*- lexical-binding: t; -*-
;; Author: Lucas Gruss
;; This file is NOT part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:

(use-package mu4e
  :straight (:type git :host github :repo "djcb/mu"
		   :pre-build (("./autogen.sh") ("make") ("sudo" "make" "install")))
  :bind (:map mu4e-compose-mode-map
	      ("<localleader> a" . #'mail-add-attachment))
  :defer t
  :commands mu4e
  :custom
  (mu4e-get-mail-command "offlineimap")
  (mu4e-update-interval 300 "Check for mail every 5 minutes")
  (message-send-mail-function 'smtpmail-send-it)
  (user-mail-address "lucas.gruss@laposte.net")
  (mu4e-compose-format-flowed t)
  (mu4e-completing-read-function #'completing-read)
  (mu4e-context-policy 'pick-first)
  (mail-user-agent 'mu4e-user-agent)
  :config
  (setq mu4e-contexts
	`(,(make-mu4e-context
	    :name "Laposte" 
	    :match-func
	    (lambda (msg)
	      (when msg
		(string-prefix-p "/Laposte" (mu4e-message-field msg :maildir))))
	    :vars
	    '(
	      (user-full-name . "Lucas Gruss")
	      (user-mail-address . "lucas.gruss@laposte.net")
	      (smtpmail-smtp-user . "lucas.gruss")
	      (smtpmail-smtp-server . "smtp.laposte.net")
	      (smtpmail-smtp-service . 465)
	      (smtpmail-stream-type . ssl)
	      (mu4e-sent-folder . "/Laposte/INBOX.Sent")
	      (mu4e-drafts-folder . "/Laposte/INBOX.Drafts")
	      (mu4e-trash-folder . "/Laposte/INBOX.TRASH")))
	  ,(make-mu4e-context
	    :name "doctorant" 
	    :match-func
	    (lambda (msg)
	      (when msg
		(string-prefix-p "/" (mu4e-message-field msg :maildir))))
	    :vars
	    '(
	      (user-full-name . "Lucas Gruss")
	      (user-mail-address . "lucas.gruss@imt-atlantique.net")
	      (smtpmail-smtp-user . "lucas.gruss@imt-atlantique.net")
	      (smtpmail-smtp-server . "z.imt.fr")
	      (smtpmail-smtp-service . 597)
	      (smtpmail-stream-type . starttls)
	      (mu4e-sent-folder . "/imt-atlantique/Sent")
	      (mu4e-drafts-folder . "/imt-atlantique/Drafts")
	      (mu4e-trash-folder . "/imt-atlantique/Trash")))
	  ,(make-mu4e-context
	   :name "ls2n"
	   :match-func
	   (lambda (msg)
	     (when msg
	       (string-prefix-p "/LS2N" (mu4e-message-field msg :maildir))))
	   :vars
	   '((user-full-name . "Lucas Gruss")
	     (user-mail-address . "lucas.gruss@ls2n.fr")
	     (smtpmail-mail-address . "lucas.gruss@ls2n.fr")
	     (smtpmail-smtp-user . "gruss-l")
	     (smtpmail-smtp-server . "smtp-tls.univ-nantes.fr")
	     (smtpmail-smtp-service . 465)
	     (smtpmail-stream-type . starttls)
	     (mu4e-sent-folder . "/LS2N/INBOX.Sent")
	     (mu4e-drafts-folder . "/LS2N/INBOX.Drafts")
	     (mu4e-trash-folder . "/LS2N/INBOX.Trash"))))))

(use-package mu4e-marker-icons
  :straight t
  :after mu4e
  :config (mu4e-marker-icons-mode +1))

(provide 'lg-mail)
;;; lg-mail.el ends here
