;; -*- lexical-binding: t; -*-
;;; init.el : where all the magic starts

;;; Add the modules to the load-path
(defvar lg/configuration-path (expand-file-name "lisp" "~/.emacs.d"))
(add-to-list 'load-path lg/configuration-path)

;;; Declare the modules to be enabled
(defvar lg/modules
  '("core"
    "private"
    "lg-ui"
    "lg-keybindings"
    "lg-transient"
    "lg-window"
    "lg-completion"
    "lg-tools"
    "lg-lang"
    "lg-mail"
    "lg-org"
    "lg-fun"
    "lg-dired"
    "lg-erc"
    ;; "lg-exwm"
    )
  "List of enabled modules in my configuration.")

(defun lg/require (package)
  "Require a package, and print a message containing the load time
on completion."
  (let ((before-load-time (float-time)))
    (require (intern package))
    (message "Loaded %s in %s." package
	     (format "%.2f seconds"
		     (float-time
		     (time-since before-load-time))))))

(defun lg/emacs-startup-time-info ()
  "Profile emacs startup."
  (message "*** Emacs loaded in %s with %d garbage collections."
	   (format "%.2f seconds"
		   (float-time
		    (time-subtract after-init-time before-init-time)))
	   gcs-done))

(add-hook 'emacs-startup-hook #'lg/emacs-startup-time-info)

;;; Require all the modules
(mapcar #'lg/require lg/modules)
