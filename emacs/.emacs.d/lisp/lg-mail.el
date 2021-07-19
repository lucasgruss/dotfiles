;; lg-mail.el : configuration for the mails

(use-package mu4e
  :ensure-system-package mu4e
  :load-path "/usr/share/emacs/site-lisp/mu4e"
  :general
  (my-leader-def
    :keymaps 'override
    "om" '(mu4e :which-key "Mail - mu4e"))
  :config
  (setq user-mail-address nil)
  (setq message-send-mail-function 'smtpmail-send-it)
  (setq mu4e-compose-format-flowed t)
  (setq mu4e-completing-read-function #'completing-read)
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
	     (user-mail-address "lucas.gruss@laposte.net")
	     (smtpmail-smtp-user . "lucas.gruss")
	     (smtpmail-smtp-server . "smtp.laposte.net")
	     (smtpmail-smtp-service . 465)
	     (smtpmail-stream-type . ssl)
	     (mu4e-sent-folder . "/Laposte/INBOX.Sent")
	     (mu4e-drafts-folder . "/Laposte/INBOX.Drafts")
	     (mu4e-trash-folder . "/Laposte/INBOX.TRASH")))
	  ,(make-mu4e-context
	   :name "Gmail"
	   :match-func
	   (lambda (msg)
	     (when msg
	       (string-prefix-p "/Laposte" (mu4e-message-field msg :maildir))))
	   :vars
	   '((user-full-name . "Lucas Gruss")
	     (user-mail-address "lucas.gruss@gmail.com")
	     (smtpmail-smtp-user . "lucas.gruss")
	     (smtpmail-smtp-server . "smtp.gmail.com")
	     (smtpmail-smtp-service . 465)
	     (smtpmail-stream-type . ssl)
	     (mu4e-sent-folder . "/Laposte/INBOX.Sent")
	     (mu4e-drafts-folder . "/Laposte/INBOX.Drafts")
	     (mu4e-trash-folder . "/Laposte/INBOX.TRASH")))))
  (setq mu4e-context-policy 'pick-first)
  (setq mail-user-agent 'mu4e-user-agent))

(provide 'lg-mail)
