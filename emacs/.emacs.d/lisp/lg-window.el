;;; lg-window.el : window (as in "emacs window") management configuration 

(use-package emacs
  :straight nil
  :init
  (setq display-buffer-alist
	`(("\\*\\(helpful\\|Help\\).*\\*"
	   (display-buffer-in-side-window)
	   (window-width . ,(+ 3 fill-column))
	   (side . right)
	   (slot . -2)))))

(use-package windmove
  :bind
  (("s-h" . windmove-left)
   ("s-j" . windmove-down)
   ("s-k" . windmove-up)
   ("s-l" . windmove-right))
  :config
  (setq windmove-wrap-around nil)
  (setq windmove-window-distance-delta 1))

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

(use-package framemove
  :load-path "~/.emacs.d/lisp/site-packages"
  :config
  (setq framemove-hook-into-windmove t))

(provide 'lg-window)
