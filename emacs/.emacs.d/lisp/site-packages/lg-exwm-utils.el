;;; lg-exwm-utils.el -*- lexical-binding: t; -*-

;; Author: Lucas Gruss (lucas.gruss@gmail.com)
;; Created: November 26, 2020
;; Keywords: exwm, convenience
;;
;; This file is not a part of GNU Emacs.
;;
;; This is a collection of functions to extend EXWM and change some of its
;; behavior
;;
;; Version 0.10
;;
;; This software is licensed under the GPL version 3.
;;
;; To install:
;;   (require 'exwm-utils)
;;
;; See README for more information

;;; Code:
(require 'exwm)
(require 'exwm-workspace)

(defun exwm-utils-workspace-switch-cyclically ()
  "Cycle through workspaces"
  (interactive)
  (other-frame +1))

(defun exwm-utils-workspace-move-cyclically ()
  "Move the current window to the next exwm workspace"
  (interactive)
  (exwm-workspace-move-window
   (or (nth (+ exwm-workspace-current-index 1) exwm-workspace--list)
       (car exwm-workspace--list)))
  (exwm-utils-workspace-switch-cyclically))

(defun exwm-utils-workspace-switch-to-buffer (buffer-or-name)
  "Extend exwm-workspace-switch-to-buffer to move the window to
  the current window instead of switching to the corresponding
  workspace. This enables x windows to behave like regular windows"
(interactive
   (let ((inhibit-quit t))
     (prog1
         (with-local-quit
           (list (get-buffer (read-buffer-to-switch "Switch to buffer: ")))))))
  (with-current-buffer buffer-or-name
    (when (derived-mode-p 'exwm-mode)
      (exwm-workspace-move-window exwm-workspace-current-index exwm--id))
    (exwm-workspace-switch-to-buffer buffer-or-name)))

(defun exwm-layout--show (id &optional window)
  "Show window ID exactly fit in the Emacs window WINDOW."
  (exwm--log "Show #x%x in %s" id window)
  (let* ((edges (window-inside-absolute-pixel-edges window))
	 (x (pop edges))
	 (y (pop edges))
	 (width (- (pop edges) x))
	 (height (- (pop edges) y))
	 frame-x frame-y frame-width frame-height)
    (with-current-buffer (exwm--id->buffer id)
      (when exwm--floating-frame
	(setq frame-width (frame-pixel-width exwm--floating-frame)
	      frame-height (+ (frame-pixel-height exwm--floating-frame)
			      ;; Use `frame-outer-height' in the future.
			      exwm-workspace--frame-y-offset))
	(when exwm--floating-frame-position
	  (setq frame-x (elt exwm--floating-frame-position 0)
		frame-y (elt exwm--floating-frame-position 1)
		x (+ x frame-x (- exwm-layout--floating-hidden-position))
		y (+ y frame-y (- exwm-layout--floating-hidden-position)))
	  (setq exwm--floating-frame-position nil))
	(exwm--set-geometry (frame-parameter exwm--floating-frame
					     'exwm-container)
			    frame-x frame-y frame-width frame-height))
      (when (exwm-layout--fullscreen-p)
	(with-slots ((x* x)
		     (y* y)
		     (width* width)
		     (height* height))
	    (exwm-workspace--get-geometry exwm--frame)
	  (setq x x*
		y y*
		width width*
		height height*)))
      ;; edited here
      (when
	  (and
	   (not (exwm-layout--fullscreen-p))
	   (not (bound-and-true-p centaur-tabs-local-mode))
	   (or (bound-and-true-p centaur-tabs-mode)
	       (bound-and-true-p tab-line-mode)))
	(setq y (+ y centaur-tabs-height)))
      ;; edited here
      (exwm--set-geometry id x y width height)
      (xcb:+request exwm--connection (make-instance 'xcb:MapWindow :window id))
      (exwm-layout--set-state id xcb:icccm:WM_STATE:NormalState)
      (setq exwm--ewmh-state
	    (delq xcb:Atom:_NET_WM_STATE_HIDDEN exwm--ewmh-state))
      (exwm-layout--set-ewmh-state id)
      (exwm-layout--auto-iconify)))
  (xcb:flush exwm--connection))

(provide 'lg-exwm-utils)
