;;; lg-ui --- Configuration for the UI of Emacs -*- lexical-binding: t; -*-
;; Author: Lucas Gruss

;;; Icons
;;;; kind-icons
(use-package kind-icon
  :straight t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;;;; all-the-icons
(use-package all-the-icons
  :straight t
  :custom
  (all-the-icons-scale-factor 0.9)
  (all-the-icons-default-alltheicon-adjust 0.0)
  :config
  (add-to-list 'all-the-icons-mode-icon-alist
	       '(exwm-mode  all-the-icons-faicon "toggle-on" :height 1.0 :v-adjust -0.2
			    :face all-the-icons-green))
  (add-to-list 'all-the-icons-mode-icon-alist
	       '(ledger-mode all-the-icons-faicon "money" :height 1.0 :v-adjust -0.2
			    :face all-the-icons-green))
  (add-to-list 'all-the-icons-icon-alist
	       '("\\.m$" all-the-icons-fileicon "matlab" :face all-the-icons-orange))
  (add-to-list 'all-the-icons-icon-alist
	       '("\\.ledger$" all-the-icons-faicon "money" :face all-the-icons-green))
  (set-fontset-font t 'unicode (font-spec :family "all-the-icons") nil 'append))

;;;; all-the-icons-dired
(use-package all-the-icons-dired
  :straight t
  :demand t
  :diminish all-the-icons-dired-mode
  :hook (dired-mode . all-the-icons-dired-mode))

;;;; all-the-icons-completion
(use-package all-the-icons-completion
  :straight t
  :defer 5
  :config (all-the-icons-completion-mode +1))

;;;; all-the-icons-ibuffer
(use-package all-the-icons-ibuffer
  :straight t
  :demand t
  :config (all-the-icons-ibuffer-mode +1))

;;; Emacs settings
(use-package emacs
  :init
  (display-battery-mode +1)
  (display-time-mode +1)
  :custom
  (display-time-format "%H:%M")
  (scroll-step 1)
  (scroll-conservatively 10000))
  ;; :config
  ;; (set-face-attribute 'default nil :family "Iosevka" :weight 'normal :height 110)
  ;; (set-face-attribute 'fixed-pitch nil :family "Iosevka" :weight 'normal :height 110)
  ;; (set-face-attribute 'variable-pitch nil :family "Roboto" :weight 'semi-light :height 110 :width 'normal))
(use-package time
  :custom
  (display-time-format (concat (when (featurep 'all-the-icons)
				 (all-the-icons-wicon "time-9")) " %H:%M"))
  (display-time-string-forms
   '((if
	 (and
	  (not display-time-format)
	  display-time-day-and-date)
	 (format-time-string "%a %b %e " now)
       #1="")
     (propertize
      (format-time-string
       (or display-time-format
	   (if display-time-24hr-format "%H:%M" "%-I:%M%p"))
       now)
      'help-echo
      (format-time-string "%a %b %e, %Y" now))))
  :config
  (display-time-mode +1))

;;;; Syncing system themes with emacs theme
(use-package lg-system-theme-sync
  :load-path "~/.emacs.d/lisp/site-packages/"
  :custom 
  (system-theme-sync-default-light-plist '(:background "/home/lucas/Images/Wallpaper/moutain.png"
					   :gtk-theme "Adwaita"
					   :icon-theme "Papirus-Light"))
  (system-theme-sync-default-dark-plist '(:background "/home/lucas/Images/Wallpaper_bis/space3.png"
					  :gtk-theme "Adwaita-dark"
				          :icon-theme "Papirus-Dark"))
  :init
  (defun load-theme--disable-old-theme(theme &rest args)
    "Disable current theme before loading new one."
    (mapcar #'disable-theme custom-enabled-themes))
  (advice-add 'load-theme :before #'load-theme--disable-old-theme)
  :config
  (system-theme-sync-mode +1))

;;;; Transparency
(use-package emacs
  :init
  (setq frame-alpha-lower-limit 1)
  (defvar lg/transparency-alpha 80
    "Transparency of all frames.")

  (defvar lg/transparency-default-increment 5
    "Default {in, de}-crement value for the transparency alpha")

  (defun lg/toggle-transparency ()
    "Toggle the transparency of Emacs on and off"
    (interactive)
    (let ((alpha (frame-parameter nil 'alpha-background)))
      (set-frame-parameter
       nil 'alpha-background
       (if (eql
	    alpha
	    ;; (cond ((numberp alpha) alpha)
	    ;; 	      ((numberp (cdr alpha)) (cdr alpha))
	    ;; 	      ;; Also handle undocumented (<active> <inactive>) form.
	    ;; 	      ((numberp (cadr alpha)) (cadr alpha)))
		100)
	   lg/transparency-alpha
	 ;;'(100 . 100)
	 100
	 ))))

  (defun lg/transparency-alpha-increase (arg)
    "Decrease transparency of the frame"
    (interactive "P")
    (let ((inc (if arg arg lg/transparency-default-increment)))
      (setq lg/transparency-alpha (+ lg/transparency-alpha inc)))
    (lg/transparency-apply))

  (defun lg/transparency-alpha-decrease (arg)
    "Increase transparency of the frame"
    (interactive "P")
    (let ((inc (if arg arg lg/transparency-default-increment)))
      (setq lg/transparency-alpha (- lg/transparency-alpha inc)))
    (lg/transparency-apply))

  (defun lg/transparency-apply ()
    "Apply the transparency parameter to the frame"
    (interactive)
    (when (< 100 lg/transparency-alpha) (setq lg/transparency-alpha 100))
    (when (> 0 lg/transparency-alpha) (setq lg/transparency-alpha 0))
    (set-frame-parameter
     nil 'alpha-background lg/transparency-alpha))

  (lg/toggle-transparency))

;;; Modeline
(use-package emacs ; modeline
  :init
  (when (featurep 'all-the-icons)
    (defvar lg/mode-line-major-mode '(:eval (all-the-icons-icon-for-buffer) " ")
      "Display an icon in the modeline")
    ;; the icons don't show if the variable is not declared risky
    (put 'lg/mode-line-major-mode 'risky-local-variable t)
    (setq-default mode-line-format
		  '("%e"
		    "    "
		    mode-line-front-space
		    mode-line-mule-info
		    mode-line-client
		    mode-line-modified
		    mode-line-remote
		    mode-line-frame-identification
		    lg/mode-line-major-mode
		    " "
		    mode-line-buffer-identification
		    "   "
		    mode-line-position
		    evil-mode-line-tag
		    (vc-mode vc-mode)
		    " "
		    mode-line-modes
		    mode-line-misc-info))))

;;;; Moody
(use-package moody
  :disabled
  :straight t
  :demand t
  :custom
  (moody-mode-line-height 20)
  (x-underline-at-descent-line t)
  :config
  (setq-default mode-line-format
    '("%e"
      mode-line-front-space
      mode-line-mule-info
      mode-line-client
      mode-line-modified
      mode-line-remote
      mode-line-frame-identification
      moody-mode-line-buffer-identification
      "   "
      mode-line-position
      evil-mode-line-tag
      (vc-mode moody-vc-mode)
      mode-line-modes)))

;;;; hide-mode-line
(use-package hide-mode-line
  :straight t)

;;; Helpful
(use-package helpful
  :straight t
  :commands (helpful-callable
	     helpful-key
	     helpful-symbol
	     helpful-variable))

;;; Outshine
(use-package outshine
  :hook (emacs-lisp-mode . outshine-mode)
  :diminish outshine-mode
  :general
  (general-def
    :keymaps 'outshine-mode-map
    :states 'normal
    [S-?\t] #'outshine-cycle
    [?\t] #'outshine-cycle)
  :straight t)

;;; Outline
(use-package outline
  :diminish outline-minor-mode
  :defer t)


;;; Themes 
;;;; modus-themes
(use-package modus-themes
  :straight t
  :custom
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs nil)
  (modus-themes-fringes nil) ; {nil,'subtle,'intense}
  (modus-themes-lang-checkers nil) ; '(straight-underline text-also background intense faint)
  (modus-themes-mode-line '(moody)) ; list '(3d moody borderless accented)
  (modus-themes-syntax nil) ; list '(faint yellow-comments green-strings alt-syntax)
  (modus-themes-hl-line '(accented)) ; list '(accented underline intense)
  (modus-themes-subtle-line-numbers nil)
  (modus-themes-paren-match '(intense bold)) ; {nil,'subtle-bold,'intense,'intense-bold}
  (modus-themes-links '(neutral-underline))
  (modus-themes-prompts '(intense gray)) ; '(intense background gray italic bold)
  (modus-themes-completions 'moderate) ; {nil,'moderate,'opinionated})
  (modus-themes-region '(accent)) ; '(accented bg-only no-extend)
  (modus-themes-diffs nil) ; {nil, 'desaturated, 'bg-only}
  (modus-themes-org-blocks 'tinted-background) ; {nil,'gray-background,'tinted-background}
  (modus-themes-variable-pitch-ui nil)
  (modus-themes-headings
   '((1 . (rainbow 1.1))
     (2 . (rainbow 1.1))
     (3 . (rainbow 1.1))
     (4 . (rainbow 1.1))
     (5 . (rainbow 1.1))
     (6 . (rainbow 1.1))
     (7 . (rainbow 1.1))
     (8 . (rainbow 1.1))
     (t . (rainbow))))
  :config
  ;; (defun lg/modus-themes-custom-faces (theme &rest args)
  ;;   (when (member theme '(modus-operandi modus-vivendi)) 
  ;;     (set-face-attribute 'mode-line-inactive nil
  ;; 			  :background (modus-themes-color 'bg-main))
  ;;     (set-face-attribute 'mode-line nil
  ;; 			  :background (modus-themes-color 'green-refine-bg))
  ;;     (set-face-attribute 'typit-wrong-char nil
  ;; 			  :foreground (modus-themes-color 'red))
  ;;     (set-face-attribute 'typit-correct-char nil
  ;; 			  :foreground (modus-themes-color 'green))
  ;;     (set-face-attribute 'typit-current-word nil
  ;; 			  :background (modus-themes-color 'blue-nuanced-bg))))

  ;; (advice-add 'load-theme :after #'lg/modus-themes-custom-faces)
  ;; (advice-remove 'load-theme #'lg/modus-themes-custom-faces)
  )

;;;; modus-themes-exporter
(use-package modus-themes-exporter
  :load-path "~/.emacs.d/lisp/"
  :after modus-themes
  :disabled t
  :ensure-system-package xsettingsd
  :config
  (defun lg/modus-theme-propagate (theme &rest args)
    "Xresources are computed based on the modus-themes-exporter
package and the programs that use the Xresources have the
settings applied to them."
    (when (member theme '(modus-operandi modus-vivendi))
      (modus-themes-exporter-export "xcolors" "~/.Xresources")))
  (advice-add #'load-theme :after #'lg/modus-theme-propagate))

;;;; Doom themes
(use-package doom-themes
  :commands load-theme
  :straight t)

;;;; spacemacs themes
(use-package spacemacs-theme
  :commands load-theme
  :straight t)

;;; solar
(use-package solar
  :custom
  (calendar-latitude 48.856613)
  (calendar-longitude 2.352222))

;;; Circadian
(use-package circadian
  :straight t
  :demand t
  :custom
  (circadian-themes '((:sunrise . modus-operandi)
		      (:sunset . modus-vivendi)))
  :config
  (circadian-setup))

;;; Dashboard
(use-package dashboard
  :straight t
  :when (featurep 'all-the-icons)
  :custom
  (dashboard-set-init-info nil)
  (dashboard-center-content t)
  (dashboard-startup-banner 'logo)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-set-navigator t)
  (dashboard-items '((recents . 5)
		     (bookmarks . 5)))
  :config
  (dashboard-setup-startup-hook))

;;; Tabs
;;;; tab-bar
(use-package tab-bar
  :bind ("s-?" . tab-bar-mode)
  :custom (tab-bar-format '(tab-bar-format-history
			    tab-bar-format-tabs-groups
			    tab-bar-separator
			    tab-bar-format-add-tab
			    ;;tab-bar-format-align-right
			    ;tab-bar-format-echo
			   ; tab-bar-format-global
			    ))
  :config
  (tab-bar-mode -1))
;;;; tab-line
(use-package lg-tab-line
  :bind
  ("s-/" . global-tab-line-mode)
  :general
  (general-def :states 'normal
    "C-t" 'tab-line-new-tab
    "gt" 'tab-line-switch-to-next-tab
    "gT" 'tab-line-switch-to-prev-tab)
  (general-def :states 'insert
    "C-t" 'tab-line-new-tab)
  :custom
  (tab-line-new-button-show t)
  (tab-line-new-button "NEW")
  (tab-line-separator '(propertize " " 'face (face-attribute 'default :foreground)))
  (tab-line-close-button-show t)
  (tab-line-switch-cycling t)
  (tab-line-new-tab-choice #'lg/tab-line-new-tab)
  (tab-line-tabs-function #'tab-line-tabs-mode-buffers)
  (tab-line-tab-name-function #'lg/tab-line-name-buffer-padded)
  (tab-line-tab-name-format-function #'lg/tab-line-tab-name-format)
  (tab-line-close-tab-function #'lg/tab-line-kill-buffer)
  :hook
  ((emms-playlist-mode
    org-ql-sidebar-buffer-setup
    dashboard-mode
    calendar-mode
    Info-mode
    ibuffer-sidebar-mode
    ibuffer-mode
    dired-mode
    dired-sidebar-mode
    pdf-outline-buffer-mode
    calc-mode
    calc-trail-mode) . lg/tab-line-local-disable)
  :config
  (defun lg/tab-line-local-disable () (interactive) (tab-line-mode -1))
  (add-hook 'window-configuration-change-hook
	    #'(lambda ()
		(dolist (window (window-list))
		  (set-window-parameter window 'tab-line-cache nil))))
  (global-tab-line-mode +1))

;;;; centaur-tabs
(use-package centaur-tabs
  :straight t
  :after evil
  :bind ("s-/" . centaur-tabs-mode)
  :bind (:map evil-normal-state-map
	 ("gt" . centaur-tabs-forward)
	 ("gT" . centaur-tabs-backward))
  :general
  (general-def :states 'normal "C-t" 'centaur-tabs--create-new-tab)
  :hook
  ((emms-playlist-mode
    org-ql-sidebar-buffer-setup
    dashboard-mode
    calendar-mode
    ibuffer-sidebar-mode
    ibuffer-mode
    dired-mode
    dired-sidebar-mode
    pdf-outline-buffer-mode
    exwm-floating-setup
    calc-mode
    calc-trail-mode) . centaur-tabs-local-mode)
  (after-load-theme . centaur-tabs-display-update)
  (after-load-theme . centaur-tabs-headline-match)
  :custom
  (centaur-tabs-style "bar")
  (centaur-tabs-set-modified-marker t)
  (centaur-tabs-set-icons t)
  (centaur-tabs-gray-out-icons t)
  (centaur-tabs-set-bar 'under)
  (centaur-tabs-set-bar nil)
  (centaur-tabs-show-navigation-buttons t)
  (centaur-tabs-show-new-tab-button t)
  (centaur-tabs-height 20)
  (centaur-tabs-bar-height 20)
  (centaur-tabs-enable-ido-completion nil)
  (centaur-tabs-cycle-scope 'tabs)
  (centaur-tabs-plain-icons nil)
  (centaur-tabs-label-fixed-length 15)
  (centaur-tabs-show-count t)
  (uniquify-separator "/")
  :config
  (centaur-tabs-mode +1))

;;;; lg-centaur-tabs : further configuration for centaur-tabs
(use-package lg-centaur-tabs
  :load-path "~/.emacs.d/lisp/site-packages/"
  :after centaur-tabs)

;;; display-line-numbers
(use-package display-line-numbers
  :hook ((prog-mode
	  ledger-mode). lg/display-line-numbers-mode-enable)
  :config
  (defun lg/display-line-numbers-mode-enable ()
    "Enable display-line-numbers"
    (interactive)
    (display-line-numbers-mode +1)
    (setq display-line-numbers 'relative)))

;;; Scrolling performances
;;;; fast-scroll
(use-package fast-scroll
  :straight t
  :diminish fast-scroll-mode
  :config
  (fast-scroll-config)
  (fast-scroll-mode +1))

;;;; scroll-on-jump
(use-package scroll-on-jump
  :demand t
  :straight   
  (scroll-on-jump :type git :host gitlab
		  :repo "ideasman42/emacs-scroll-on-jump")
  :after evil
  :config
  (scroll-on-jump-advice-add evil-undo)
  (scroll-on-jump-advice-add evil-redo)
  (scroll-on-jump-advice-add evil-jump-item)
  (scroll-on-jump-advice-add evil-jump-forward)
  (scroll-on-jump-advice-add evil-jump-backward)
  (scroll-on-jump-advice-add evil-search-next)
  (scroll-on-jump-advice-add evil-search-previous)
  (scroll-on-jump-advice-add evil-forward-paragraph)
  (scroll-on-jump-advice-add evil-backward-paragraph)
  ;; Actions that themselves scroll.
  (scroll-on-jump-with-scroll-advice-add evil-goto-line)
  (scroll-on-jump-with-scroll-advice-add evil-goto-first-line)
  (scroll-on-jump-with-scroll-advice-add evil-scroll-down)
  (scroll-on-jump-with-scroll-advice-add evil-scroll-up)
  (scroll-on-jump-with-scroll-advice-add evil-scroll-line-to-center)
  (scroll-on-jump-with-scroll-advice-add evil-scroll-line-to-top)
  (scroll-on-jump-with-scroll-advice-add evil-scroll-line-to-bottom)
  :custom
  (scroll-on-jump-smooth t)
  (scroll-on-jump-use-curve t)
  (scroll-on-jump-duration 0.2))

;;;; good-scroll
(use-package good-scroll
  :disabled t
  :straight t
  :config
  (good-scroll-mode +1))

;;;; pixel scroll mode
(use-package pixel-scroll
  :straight nil
  :config (pixel-scroll-mode +1))

;;; hl-line
(use-package hl-line
  :straight nil
  :config
  (global-hl-line-mode +1))

;;; LIN
(use-package lin
  :straight (:host gitlab :repo "protesilaos/lin")
  :commands lin-mode
  :config (lin-add-to-many-modes))
  
;;; hl-todo
(use-package hl-todo
  :straight t
  :config
  (global-hl-todo-mode +1))

;;; git-gutter-fringe
(use-package git-gutter-fringe
  :diminish (global-git-gutter-mode git-gutter-mode)
  :straight t
  :config
  (global-git-gutter-mode +1))

;;; visual-fill-column
(use-package visual-fill-column
  :defer t
  :straight t
  :custom
  (visual-fill-column-width 130)
  (visual-fill-column-center-text t)
  :init
  (defun lg/activate-visual-fill-center ()
    (visual-fill-column-mode +1))
  (defun lg/deactivate-visual-fill-center ()
    (visual-fill-column-mode -1))
  (defun lg/toggle-visual-fill-center ()
    (interactive)
    (if visual-fill-column-mode
	(visual-fill-column-mode -1)
      (visual-fill-column-mode +1)))
  :hook
  ((Info-mode
    org-mode
    minibuffer-mode
    ledger-mode) . lg/activate-visual-fill-center))

;;; page-break-lines
(use-package page-break-lines
  :straight t
  :diminish page-break-lines-mode
  :config
  (global-page-break-lines-mode +1))

;;; Eros : Evaluation Result Overlays 
(use-package eros
  :straight t
  :config (eros-mode +1))

;;; Sublimity
(use-package sublimity
  :straight t)

;;; rainbow mode
(use-package rainbow-mode
  :straight t
  :diminish rainbow-mode
  :hook (prog-mode . rainbow-mode))

;;; emojify
(use-package emojify
  :straight t
  :hook (erc-mode . emojify-mode)
  :commands emojify-mode)

;;; beacon
(use-package beacon
  :straight t
  :diminish beacon-mode
  :custom
  (beacon-blink-when-point-moves-vertically 1)
  (beacon-blink-when-buffer-changes t)
  (beacon-blink-when-window-changes t)
  (beacon-blink-when-window-scrolls t)
  (beacon-blink-when-focused t)
  :config
  (beacon-mode +1))

(provide 'lg-ui)
