;;; init.el : where all the magic starts

(defvar lg/configuration-path (expand-file-name "lisp" "~/.emacs.d"))
(add-to-list 'load-path lg/configuration-path)

(defvar lg/modules
  '("core"
    "private"
    "lg-ui"
    "lg-keybindings"
    "lg-window"
    "lg-completion"
    "lg-tools"
    "lg-mail"
    "lg-org"
    "lg-exwm")
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
  "Profile emacs startup"
  (message "*** Emacs loaded in %s with %d garbage collections."
	   (format "%.2f seconds"
		   (float-time
		    (time-subtract after-init-time before-init-time)))
	   gcs-done))

(add-hook 'emacs-startup-hook #'lg/emacs-startup-time-info)

(mapcar #'lg/require lg/modules)
