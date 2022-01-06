;;; lg-webkit --- configuration for the emacs-webkit web browser -*- lexical-binding: t; -*-
;; Author: Lucas Gruss

;;; Emacs-webkit
(use-package webkit
  :demand t
  :straight
  '(webkit :host github :repo "akirakyle/emacs-webkit"
	   :branch "main"
	   :files (:defaults "*.js" "*.css" "*.so")
	   :pre-build "make")
  :config
  (modify-frame-parameters nil '((inhibit-double-buffering . t)))
  (defun webkit--adjust-size (frame)
    (ignore frame)
    (dolist (buffer webkit--buffers)
      (when (buffer-live-p buffer)
	(with-current-buffer buffer
	  (let* ((windows (get-buffer-window-list buffer 'nomini t)))
	    (if (not windows)
		(webkit--hide webkit--id)
	      (let* ((show-window (if (memq (selected-window) windows)
				      (selected-window)
				    (car windows)))
		     (hide-windows (remq show-window windows))
		     (show-frame (window-frame show-window)))
		(webkit--move-to-x-or-pgtk-frame show-frame)
		(pcase-let ((`(,left ,top ,right ,bottom)
			     (window-inside-pixel-edges show-window)))
		  (webkit--show webkit--id)
		  (if (and (bound-and-true-p centaur-tabs-mode))
		      (webkit--resize webkit--id left (+ top centaur-tabs-height)
				      (- right left) (- bottom top))
		    (webkit--resize webkit--id left top
				    (- right left) (- bottom top)))
		  (dolist (window hide-windows)
		    (switch-to-prev-buffer window)))))))))))

(use-package webkit-ace)
(use-package webkit-dark)
