;;; lg-ui --- Configuration for the UI of Emacs -*- lexical-binding: t; -*-
;; Author: Lucas Gruss

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

;;;; Syncing system themes with emacs theme
(use-package emacs
  ;:hook (after-init . lg/load-theme)
  :init
  ;; https://github.com/ema2159/centaur-tabs/issues/157
  (defvar after-load-theme-hook nil
    "Hook run after a color theme is loaded using `load-theme'.")
  (defadvice load-theme (after run-after-load-theme-hook activate)
    "Run `after-load-theme-hook'."
    (run-hooks 'after-load-theme-hook))

  (defvar lg/light-themes
    '(modus-operandi
      doom-solarized-light
      spacemacs-light)
    "List of light themes.")

  (defvar lg/dark-themes
    '(modus-vivendi
      doom-one
      doom-dark+
      doom-xcode
      doom-badger
      doom-1337
      doom-dracula
      spacemacs-dark)
    "List of dark themes.")

  (defvar lg/theme 'modus-operandi)

  (defvar lg/background)
  (defvar lg/gtk-theme)
  (defvar lg/icon-theme)

  (defvar lg/light-background "/home/lucas/Images/Wallpaper/moutain.png" "A background to be used when theme is light.")
  (defvar lg/light-gtk-theme "Adwaita" "Light gtk-theme to apply.")
  (defvar lg/light-icon-theme "Papirus-Light" "Light icon-theme to apply.")
  (defvar lg/dark-background "/home/lucas/Images/Wallpaper_bis/space3.png" "A background to be used when theme is dark.")
  (defvar lg/dark-gtk-theme "Adwaita-dark" "Dark gtk-theme to apply.")
  (defvar lg/dark-icon-theme "Papirus-Dark" "Dark icon-theme to apply.")

  (defun lg/theme-apply (gtk-theme icon-theme background-img)
    "Apply GTK-THEME and ICON-THEME to the system."
    (setq lg/background background-img)
    (setq lg/gtk-theme gtk-theme)
    (setq lg/icon-theme icon-theme)
    (efs/run-in-background (format "xfconf-query -c xfce4-desktop -p  /backdrop/screen0/monitoreDP-1/workspace0/last-image -s %s" background-img))
    (efs/run-in-background (format "xfconf-query -c xsettings -p /Net/ThemeName -s %s" gtk-theme))
    (efs/run-in-background (format "xfconf-query -c xsettings -p /Net/IconThemeName -s %s" icon-theme))
    (shell-command (format "sed -i 's/ThemeName.*/ThemeName \"%s\"/g' ~/.xsettingsd" gtk-theme))
    (shell-command (format "sed -i 's/IconThemeName.*/IconThemeName \"Papirus-Light\"/g' ~/.xsettingsd" icon-theme)))

  (defun lg/theme-propagate (theme &rest args)
    "Apply system wide settings that are consistent with the emacs
themes. Suitable gtk and icon themes are applied, and some colors
are changed in the Xresources file.

This function is to be used with xsettingsd, but the same can be
applied to gnome-settings or xfce-conf."
    (when (member theme lg/light-themes)
      (lg/theme-apply lg/light-gtk-theme lg/light-icon-theme lg/light-background))
    (when (member theme lg/dark-themes)
      (lg/theme-apply lg/dark-gtk-theme lg/dark-icon-theme lg/dark-background))
    (shell-command (format "sed -i 's/background =.*/background = \"%s\"/g' ~/.config/dunst/dunstrc" (face-foreground 'default)))
    (shell-command (format "sed -i 's/foreground =.*/foreground = \"%s\"/g' ~/.config/dunst/dunstrc" (face-background 'default)))
    (efs/run-in-background "killall dunst")
    (shell-command (format "sed -i 's/\*background.*/\*background\: %s/g' ~/.Xresources" (face-background 'default)))
    (shell-command (format "sed -i 's/\*foreground.*/\*foreground\: %s/g' ~/.Xresources" (face-foreground 'default)))
    (efs/run-in-background "killall -HUP xsettingsd")
    (shell-command "xrdb ~/.Xresources"))

  (advice-add 'load-theme :after #'lg/theme-propagate '(depth 100))

  (defun load-theme--disable-old-theme(theme &rest args)
    "Disable current theme before loading new one."
    (mapcar #'disable-theme custom-enabled-themes))

  (defun lg/load-theme ()
    "Load the theme defined in lg/theme"
    (load-theme lg/theme t nil))

  (advice-add 'load-theme :before #'load-theme--disable-old-theme))

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
    (let ((alpha (frame-parameter nil 'alpha)))
      (set-frame-parameter
       nil 'alpha
       (if (eql (cond ((numberp alpha) alpha)
		      ((numberp (cdr alpha)) (cdr alpha))
		      ;; Also handle undocumented (<active> <inactive>) form.
		      ((numberp (cadr alpha)) (cadr alpha)))
		100)
	   lg/transparency-alpha '(100 . 100)))))

  (defun lg/transparency-alpha-increase (arg)
    "Increase transparency of the frame"
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
     nil 'alpha lg/transparency-alpha))

  (lg/toggle-transparency))

;;; Modeline
;;;; Moody
(use-package moody
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
					;(moody-replace-mode-line-buffer-identification)
					;(moody-replace-vc-mode))

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
  (general-def :keymaps 'outshine-mode-map :states 'normal
    [S-?\t] #'outshine-cycle)
  :straight t)

;;; Outline
(use-package outline
  :diminish outline-minor-mode
  :defer t)

;;; Icons
;;;; all-the-icons
(use-package all-the-icons
  :straight t
  :defer t
  :custom
  (all-the-icons-scale-factor 1.0)
  :config
  (add-to-list 'all-the-icons-mode-icon-alist
               '(exwm-mode  all-the-icons-faicon "toggle-on" :height 1.0 :v-adjust -0.2
                            :face all-the-icons-green))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.m$" all-the-icons-fileicon "matlab" :face all-the-icons-orange)))

;;;; all-the-icons-dired
(use-package all-the-icons-dired
  :straight t
  :after dired
  :diminish all-the-icons-dired-mode
  :hook (dired-mode . all-the-icons-dired-mode))

;;;; all-the-icons-completion
(use-package all-the-icons-completion
  :straight t
  :after all-the-icons
  :config (all-the-icons-completion-mode +1))

;;;; all-the-icons-ibuffer
(use-package all-the-icons-ibuffer
  :straight t
  :config (all-the-icons-ibuffer-mode +1))

;;; Themes 
;;;; modus-themes
(use-package modus-themes
  :straight t
  :custom
  (modus-themes-slanted-constructs t)
  (modus-themes-bold-constructs nil)
  (modus-themes-fringes nil) ; {nil,'subtle,'intense})
  ;; Options for `modus-themes-lang-checkers': nil,
  ;; 'straight-underline, 'subtle-foreground,
  ;; 'subtle-foreground-straight-underline, 'intense-foreground,
  ;; 'intense-foreground-straight-underline, 'colored-background
  (modus-themes-lang-checkers 'straight-underline)

  ;; Options for `modus-themes-mode-line': nil, '3d, 'moody,
  ;; 'borderless, 'borderless-3d, 'borderless-moody, 'accented,
  ;; 'accented-3d, 'accented-moody
  (modus-themes-mode-line 'accented-moody)

  ;; Options for `modus-themes-syntax': nil, 'faint,
  ;; 'yellow-comments, 'green-strings,
  ;; 'yellow-comments-green-strings, 'alt-syntax,
  ;; 'alt-syntax-yellow-comments, 'faint-yellow-comments
  (modus-themes-syntax nil)

  ;; Options for `modus-themes-hl-line': nil, 'intense-background,
  ;; 'accented-background, 'underline-neutral,
  ;; 'underline-accented, 'underline-only-neutral,
  ;; 'underline-only-accented
  (modus-themes-intense-hl-line 'accented-background)
  (modus-themes-subtle-line-numbers nil)
  (modus-themes-paren-match '(intense bold)) ; {nil,'subtle-bold,'intense,'intense-bold}

  ;; Options for `modus-themes-links': nil, 'faint,
  ;; 'neutral-underline, 'faint-neutral-underline, 'no-underline,
  ;; 'underline-only, 'neutral-underline-only
  (modus-themes-links 'neutral-underline)

  ;; Options for `modus-themes-prompts': nil, 'subtle-accented,
  ;; 'intense-accented, 'subtle-gray, 'intense-gray
  (modus-themes-prompts 'intense-gray)
  (modus-themes-completions 'moderate) ; {nil,'moderate,'opinionated})

  ;; Options for `modus-themes-region': nil, 'no-extend, 'bg-only,
  ;; 'bg-only-no-extend, 'accent, 'accent-no-extend
  (modus-themes-region 'accent)

  ;; Options for `modus-themes-diffs': nil, 'desaturated,
  ;; 'fg-only, 'bg-only, 'deuteranopia,
  (modus-themes-diffs nil) ;
  (modus-themes-org-blocks 'rainbow) ; {nil,'greyscale,'rainbow}
  (modus-themes-org-habit nil) ; {nil,'simplified,'traffic-light}
  (modus-themes-headings '((t . highlight)))
  (modus-themes-variable-pitch-ui nil)
  (modus-themes-variable-pitch-headings t)
  (modus-themes-scale-headings t)
  (modus-themes-scale-1 1.1)
  (modus-themes-scale-2 1.15)
  (modus-themes-scale-3 1.21)
  (modus-themes-scale-4 1.27)
  (modus-themes-scale-5 1.33)

  :config
  (defun lg/modus-themes-custom-faces (theme &rest args)
    (message (stringp theme))
    (when (member theme '(modus-operandi modus-vivendi)) 
      (set-face-attribute 'mode-line-inactive nil
			  :background (modus-themes-color 'bg-main))
      (set-face-attribute 'mode-line nil
			  :background (modus-themes-color 'green-refine-bg))
      ;; tabs
      ;; (set-face-attribute 'tab-line nil
      ;; 			  :background (modus-themes-color 'bg-main))
      ;; (set-face-attribute 'tab-bar nil
      ;; 			  :background (modus-themes-color 'bg-main))
      ;; (set-face-attribute 'centaur-tabs-selected nil
      ;; 			  :background (face-attribute 'mode-line :background)
      ;; 			  :box nil)
      ;; (set-face-attribute 'centaur-tabs-default nil
      ;; 			  :background (face-attribute 'mode-line :background)
      ;; 			  :box nil)
      ;; (set-face-attribute 'centaur-tabs-unselected nil
      ;; 			  :background (modus-themes-color 'bg-alt)
      ;; 			  :box nil)
      ))

  (advice-add 'load-theme :after #'lg/modus-themes-custom-faces))

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

;;;; solar
(use-package solar
  :custom
  (calendar-latitude 48.856613)
  (calendar-longitude 2.352222))

;;;; Circadian
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
;  :after circadian
  ;:demand t
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
			    tab-bar-format-align-right
			    ;tab-bar-format-echo
			    tab-bar-format-global
			    ))
  :config
  (defun tab-bar-format-echo ()
    `((global menu-item ,(string-trim-right (with-current-buffer (get-buffer " *Echo Area 1*")
					      (buffer-string))) ignore)))
  (tab-bar-mode -1))

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
  :hook (prog-mode . lg/display-line-numbers-mode-enable)
  :config
  (defun lg/display-line-numbers-mode-enable ()
    "Enable display-line-numbers"
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
    minibuffer-mode) . lg/activate-visual-fill-center))

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
