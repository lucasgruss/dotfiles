;;; init.el : where all the magic starts -*- lexical-binding: t; -*-

;;; https://github.com/kiwanami/emacs-epc/issues/35
(setq byte-compile-warnings '(cl-functions))

;;; Add the modules to the load-path
(defvar lg/configuration-path (expand-file-name "lisp" "~/.emacs.d"))
(add-to-list 'load-path lg/configuration-path)

;;; Declare the modules to be enabled
(defvar lg/modules
  '("core"
    "private"
    "lg-keybindings"
    "lg-ui"
    "lg-transient"
    "lg-window"
    "lg-completion"
    "lg-tools"
    "lg-shell"
    "lg-lang"
    "lg-mail"
    "lg-org"
    "lg-fun"
    "lg-dired"
    "lg-erc"
    ;; "lg-exwm"
    )
  "List of enabled modules in my configuration.")

(defun lg/emacs-startup-time-info ()
  "Profile emacs startup."
  (message "*** Emacs loaded in %s with %d garbage collections."
	   (format "%.2f seconds"
		   (float-time
		    (time-subtract after-init-time before-init-time)))
	   gcs-done))

(add-hook 'emacs-startup-hook #'lg/emacs-startup-time-info)

;;; Require all the modules
(dolist (module lg/modules)
  (let ((before-load-time (float-time)))
    (require (intern module))
    (message "Loaded %s in %s." module
	     (format "%.2f seconds"
		     (float-time
		     (time-since before-load-time))))))
