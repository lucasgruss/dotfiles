;; -*- lexical-binding: t; -*-
;;; lg-window.el : window (as in "emacs window") management configuration 

;;; Emacs settings
(use-package emacs ;; display-buffer-alist
  :straight nil
  :bind
  (("s-v" . split-window-right)
   ("s-z" . split-window-below)
   ("s-q" . delete-window))
  :custom
  (display-buffer-alist
   `(("\\*\\(helpful\\|Help\\|MATLAB Help\\).*\\*"
      (display-buffer-in-side-window)
      (window-width . ,(+ 3 fill-column))
      (side . right)
      (slot . -2))
     ("\\*\\(Ledger Report\\).*\\*"
      (display-buffer-in-side-window)
      (window-width . ,(+ 3 fill-column))
      (side . right)
      (slot . -2))
     ("\\*\\(system-packages\\).*\\*"
      (display-buffer-in-side-window)
      (window-width . ,(+ 3 fill-column))
      (side . right)
      (slot . -2))
     ("*elfeed-entry*"
      (display-buffer-in-direction)
      (direction . below))
     ("\\*\\Outline .*\.pdf\\*"
      (display-buffer-in-direction)
      (direction . right)
      (window-width . 0.2))
     ("Browsing by: .*"
      (display-buffer-in-direction)
      (direction . left)
      (window-width . 0.15))
     ("\\*org-roam\\*"
      (display-buffer-in-side-window)
      (side . right)
      (slot . 0)
      (window-width . 0.33)
      (window-parameters . ((no-other-window . t)
			    (no-delete-other-windows . t))))
     ("*Music*"
      (display-buffer-in-direction)
      (direction . left)
      (window-width . 0.15))))
  :config
  (defun lg/toggle-all-frames-fullscreen ()
    (interactive)
    (lambda () (mapc 'toggle-frame-fullscreen (frame-list)))))

;;; popper
(use-package popper
  :straight t
  :custom
  (popper-display-control nil) 
  (popper-reference-buffers
   '("\\*Messages\\*"
     "Output\\*$"
     emms-playlist-mode
     emms-browser-mode
     ;help-mode
     ;compilation-mode
     ))
  :config
  (popper-mode +1))

;;; Winner mode
(use-package winner
  :config
  (winner-mode +1))

;;; Windmove
(use-package windmove
  :bind
  (("s-h" . windmove-left)
   ("s-j" . windmove-down)
   ("s-k" . windmove-up)
   ("s-l" . windmove-right))
  :custom
  (windmove-wrap-around nil)
  (windmove-window-distance-delta 1))

;;; Windower
(use-package windower
  :straight t
  :init
  (setq windower-border-move-distance 1)
  :bind
  (("s-H" . windower-swap-left)
   ("s-J"    . windower-swap-below)
   ("s-K"    . windower-swap-above)
   ("s-L"    . windower-swap-right)
   ("s-M-h"  . windower-move-border-left)
   ("s-M-j"  . windower-move-border-below)
   ("s-M-k"  . windower-move-border-above)
   ("s-M-l"  . windower-move-border-right)
   ("s-<tab>" . windower-switch-to-last-buffer)
   ("s-r"    . windower-switch-to-last-buffer)
   ("s-o"    . windower-toggle-single)
   ("s-\\"   . windower-toggle-split)))

;;; Framemove
(use-package framemove
  ;:load-path "~/.emacs.d/lisp/site-packages"
  :demand t
  :after windmove
  :custom
  (framemove-hook-into-windmove t))

(provide 'lg-window)
