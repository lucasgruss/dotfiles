;;; init.el : where all the magic starts

(defvar lg/configuration-path (expand-file-name "lisp" "~/.emacs.d"))
(add-to-list 'load-path lg/configuration-path)

(defvar lg/modules
  '("core"
    "lg-keybindings"
    "lg-window"
    "lg-completion"
    "lg-ui"
    "lg-tools"
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

(mapcar #'lg/require lg/modules)

