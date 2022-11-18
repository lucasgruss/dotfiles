;;; lg-ui --- Configuration for the UI of Emacs -*- lexical-binding: t; -*-
;; Author: Lucas Gruss
;; This file is NOT part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:

;;; Icons
(use-package kind-icon
  :straight t
  :after corfu
  :hook (system-theme-sync-theme . (lambda ()
				     (interactive)
				     (kind-icon-reset-cache)))
  :custom (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package all-the-icons
  :straight t
  :custom (all-the-icons-scale-factor 0.9)
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

(use-package all-the-icons-dired
  :straight t
  :demand t
  :diminish all-the-icons-dired-mode
  :hook (dired-mode . all-the-icons-dired-mode)
  :custom (all-the-icons-dired-monochrome nil))

(use-package all-the-icons-ibuffer
  :straight t
  :demand t
  :hook (ibuffer-mode . all-the-icons-ibuffer-mode))

(use-package all-the-icons-completion
  :straight t
  :demand t
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :config (all-the-icons-completion-mode))

;;; Modeline
(use-package emacs ; modeline
  :init
  (defvar lg/mode-line-major-mode
    (if (featurep 'all-the-icons)
	'(:eval (all-the-icons-icon-for-buffer) " ")
      nil)
    "Display an icon corresponding to major mode in the modeline")

  (defvar lg/mode-line-vc-mode
    (if (featurep 'all-the-icons)
	'(:eval (all-the-icons-octicon "git-branch"))
      nil)
    "Display an icon in front of git in the modeline")

  ;; the icons don't show if the variable is not declared risky
  (put 'lg/mode-line-major-mode 'risky-local-variable t)
  (put 'lg/mode-line-vc-mode 'risky-local-variable t)

  (defun lg/mode-line-render (left center right)
    "Return a string of `window-width' length.
Containing LEFT, CENTER and RIGHT aligned respectively."
    (let ((available-width
	   (- (window-total-width)
	      (+ (length (format-mode-line left))
		 (length (format-mode-line center))
		 (length (format-mode-line right))))))
      (append left
	      (list (format (format "%%%ds" (/ available-width 2)) ""))
	      center
	      (list (format (format "%%%ds" (/ available-width 2)) ""))
	      right)))

  (setq-default mode-line-format
		'((:eval
		   (lg/mode-line-render
		    ;; left
		    '("%e"
		      mode-line-front-space
		      evil-mode-line-tag
		      mode-line-mule-info
		      mode-line-client
		      mode-line-modified
		      mode-line-remote
		      mode-line-frame-identification
		      lg/mode-line-major-mode " "
		      mode-line-buffer-identification)
		    ;; center
		    nil
		    ;; right
		    '("%p"
		      (vc-mode vc-mode) " "
		      mode-line-modes
		      mode-line-misc-info
		      mode-line-end-spaces))))))

(use-package moody
  :disabled
  :straight t
  :demand t
  :custom (moody-mode-line-height 20)
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
		  lg/mode-line-major-mode
		  " "
		  moody-mode-line-buffer-identification
		  "   "
		  mode-line-position
		  evil-mode-line-tag
		  (vc-mode moody-vc-mode)
		  mode-line-modes
		  mode-line-misc-info)))

(use-package fancy-battery
  :disabled t
  :straight t
  :defer 10
  :custom (fancy-battery-show-percentage t)
  :config
  (when (featurep 'all-the-icons)
    (defun fancy-battery-default-mode-line ()
      "Assemble a mode line string for Fancy Battery Mode.
Display the remaining battery time, if available and
`fancy-battery-show-percentage' is non-nil, otherwise the
percentage.  If the battery is critical, use
`battery-critical-face'.  Otherwise use `fancy-battery-charging'
or `fancy-battery-discharging', depending on the current
state. An icon is also shown for eye candy."
      (when fancy-battery-last-status
	(let* ((time (cdr (assq ?t fancy-battery-last-status)))
	       (percentage (cdr (assq ?p fancy-battery-last-status)))
	       (percentage-int (string-to-number percentage))
	       (face (if (>= percentage-int 98)
			 'fancy-battery-charging
		       (pcase (cdr (assq ?b fancy-battery-last-status))
			 ("!" 'fancy-battery-critical)
			 ("+" 'fancy-battery-charging)
			 (_ 'fancy-battery-discharging))))
	       (icon (cond
		      ((or (>= percentage-int 99) (equal face 'fancy-battery-charging))
		       (all-the-icons-alltheicon "battery-charging" :face face :v-adjust 0.025 :height 1.5))
		      ((>= percentage-int 90)
		       (all-the-icons-faicon"battery-full" :face face :v-adjust 0.025 :height 1.25))
		      ((>= percentage-int 62.5)
		       (all-the-icons-faicon "battery-three-quarters" :face face :v-adjust 0.025 :height 1.25))
		      ((>= percentage-int 37.5)
		       (all-the-icons-faicon "battery-half" :face face :v-adjust 0.025 :height 1.25))
		      ((>= percentage-int 20)
		       (all-the-icons-faicon "battery-quarter" :face face :v-adjust 0.025 :height 1.25))
		      (t
		       (all-the-icons-faicon "battery-empty" :face face :v-adjust 0.025 :height 1.25))))
	       (status (if (or fancy-battery-show-percentage (string= time "N/A"))
			   (and percentage (concat percentage "%%"))
			 time)))
	  (concat
	   " " icon " "
	   (if status
	       (propertize status 'face face
			   'help-echo (format "Il reste %s temps de charge" time))
	     ;; Battery status is not available
	     (propertize "N/A" 'face 'error)))))))
  (fancy-battery-mode +1))

(use-package hide-mode-line
  :straight t)

(use-package helpful
  :straight t
;  :init
;  (defvar read-symbol-positions-list nil)
					;(defvar read-symbol-positions nil)
  :commands (helpful-callable
	     helpful-key
	     helpful-symbol
	     helpful-variable))
 ;;  :config
 ;;  ;; TODO: Remove once fixed upstream in helpful https://github.com/Wilfred/helpful/issues/282
 ;;  (defun helpful--autoloaded-p (sym buf)
 ;;    "Return non-nil if function SYM is autoloaded."
 ;;    (-when-let (file-name (buffer-file-name buf))
 ;;      (setq file-name (s-chop-suffix ".gz" file-name))
 ;;      (help-fns--autoloaded-p sym)))

 ;; (defun helpful--skip-advice (docstring)
 ;;   "Remove mentions of advice from DOCSTRING."
 ;;   (let* ((lines (s-lines docstring))
 ;; 	  (relevant-lines
 ;; 	   (--take-while
 ;; 	    (not (or (s-starts-with-p ":around advice:" it)
 ;; 		     (s-starts-with-p "This function has :around advice:" it)))
 ;; 	    lines)))
 ;;     (s-trim (s-join "\n" relevant-lines)))))


;;; Themes
(use-package lg-system-theme-sync
  :load-path "~/.emacs.d/lisp/site-packages/"
  ;:ensure-system-package papirus-icon-theme
  :demand t
  :custom
  (system-theme-sync-default-light-plist '(;:background "/home/lucas/Images/Wallpaper/nature-mountain.jpg"
						       :gtk-theme "Adwaita"
						       :icon-theme "Papirus-Light"))
  (system-theme-sync-default-dark-plist '(;:background "/home/lucas/Images/Wallpaper_bis/space3.png"
						      :gtk-theme "Adwaita-dark"
						      :icon-theme "Papirus-Dark"))
  :hook (system-theme-sync-theme . lg/system-theme-sync-update-xresources)
  :init
  (defun lg/system-theme-sync-update-xresources ()
    (shell-command (format "sed -i 's/\*background.*/\*background\: %s/g' ~/.Xresources" (face-background 'default)))
    (shell-command (format "sed -i 's/\*foreground.*/\*foreground\: %s/g' ~/.Xresources" (face-foreground 'default)))
    (shell-command "xrdb ~/.Xresources"))
  (defun load-theme--disable-old-theme(theme &rest args)
    "Disable current theme before loading new one."
    (mapcar #'disable-theme custom-enabled-themes))
  (advice-add 'load-theme :before #'load-theme--disable-old-theme)
  :config (system-theme-sync-mode +1))

(use-package emacs ; transparency
  :init
  (setq frame-alpha-lower-limit 1)
  (defvar lg/transparency-alpha 80
    "Transparency of all frames.")

  (defvar lg/transparency-default-increment 5
    "Default {in, de}-crement value for the transparency alpha")

  (defun lg/toggle-frame-decorations ()
    (interactive)
    (set-frame-parameter nil 'undecorated (not (frame-parameter nil 'undecorated))))

  (defun lg/toggle-transparency ()
    "Toggle the transparency of Emacs on and off"
    (interactive)
    (let ((alpha (frame-parameter nil 'alpha-background)))
      (set-frame-parameter nil 'alpha-background
			   (if (eql alpha 100)
			       lg/transparency-alpha
			     100))))

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

(use-package modus-themes
  :straight t
  :custom
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs nil)
  (modus-themes-fringes nil) ; {nil,'subtle,'intense}
  (modus-themes-lang-checkers nil) ; '(straight-underline text-also background intense faint)
  (modus-themes-mode-line '(2 borderless)) ; list '(3d moody borderless accented)
  (modus-themes-syntax nil) ; list '(faint yellow-comments green-strings alt-syntax)
  (modus-themes-hl-line '(accented)) ; list '(accented underline intense)
  (modus-themes-subtle-line-numbers nil)
  (modus-themes-paren-match '(intense bold)) ; {nil,'subtle-bold,'intense,'intense-bold}
  (modus-themes-links '(neutral-underline))
  (modus-themes-prompts '(intense gray)) ; '(intense background gray italic bold)
  (modus-themes-completions '((matches . (extrabold))
			      (selection . nil)
			      (popup . (accented))))
  (modus-themes-region '(accent)) ; '(accented bg-only no-extend)
  (modus-themes-diffs nil) ; {nil, 'desaturated, 'bg-only}
  (modus-themes-org-blocks 'tinted-background) ; {nil,'gray-background,'tinted-background}
  (modus-themes-variable-pitch-ui nil)
  (modus-themes-org-agenda
   '((header-date . (grayscale))))
  ;; (modus-themes-headings
  ;;  '((1 . (rainbow 1.9))
  ;;    (2 . (rainbow 1.6))
  ;;    (3 . (rainbow 1.3))
  ;;    (t . (rainbow 1.1))))
  (modus-themes-headings
   '((1 . (rainbow 1))
     (2 . (rainbow 1))
     (3 . (rainbow 1))
     (t . (rainbow 1)))))

(use-package modus-themes-exporter
  :disabled t
  :load-path "~/.emacs.d/lisp/"
  :after modus-themes
  :ensure-system-package xsettingsd
  :config
  (defun lg/modus-theme-propagate (theme &rest args)
    "Xresources are computed based on the modus-themes-exporter
package and the programs that use the Xresources have the
settings applied to them."
    (when (member theme '(modus-operandi modus-vivendi))
      (modus-themes-exporter-export "xcolors" "~/.Xresources")))
  (advice-add #'load-theme :after #'lg/modus-theme-propagate))

(use-package doom-themes
  :straight t
  :commands load-theme)

(use-package spacemacs-theme
  :straight t
  :commands load-theme)

(use-package github-dark-vscode-theme
  :straight t
  :commands load-theme)

(use-package kaolin-themes
  :straight t)

(use-package ef-themes
  :straight (:host github :repo "protesilaos/ef-themes")
  :commands load-theme)

(use-package circadian
  :straight t
  :after solar
  :custom
  (circadian-themes '((:sunrise . modus-operandi)
		      (:sunset . modus-vivendi)))
  :config
  (circadian-setup))

;;; Tabs
(use-package tab-bar
  :bind ("s-?" . 'toggle-frame-tab-bar)
  :custom
  (tab-bar-show nil "Hide the tab-bar by default")
  (tab-bar-menu-bar-button `(if (featurep 'all-the-icons)
			       (concat " " (all-the-icons-icon-for-mode 'emacs-lisp-mode :height 1.1 :v-adjust 0.05) " ")
			       "Menu"))
  (tab-bar-format '(tab-bar-format-menu-bar
			    tab-bar-format-history
			    tab-bar-format-tabs
			    tab-bar-separator
			    tab-bar-format-add-tab
			    tab-bar-format-align-right
			    tab-bar-format-global))
  :config
  (tab-bar-mode +1))

(use-package lg-tab-line
  :demand t
  :bind ("s-/" . global-tab-line-mode)
  :general
  (general-def :states 'normal
    "C-t" 'tab-line-new-tab
    "gt" 'tab-line-switch-to-next-tab
    "gT" 'tab-line-switch-to-prev-tab)
  (general-def :states 'insert
    "C-t" 'tab-line-new-tab)
  :custom
  (tab-line-new-button-show t)
  (tab-line-separator " ")
  (tab-line-close-button-show t)
  (tab-line-tab-max-width 20)
  (tab-line-switch-cycling t)
  (tab-line-new-tab-choice #'lg/tab-line-new-tab "Context aware new tab behaviour.")
  (tab-line-close-tab-function #'lg/tab-line-kill-buffer)
  (tab-line-tabs-function #'tab-line-tabs-mode-buffers "Group buffers by major mode.")
  (tab-line-tab-name-function #'lg/tab-line-name-buffer-padded "Tabs have all the same width.")
  (tab-line-tab-name-format-function #'lg/tab-line-tab-name-format "Add icon to the beginning of the tab.")
  (tab-line-exclude-modes '(emms-playlist-mode
			    emms-browser-mode
			    org-ql-sidebar-buffer-setup
			    dashboard-mode
			    bluetooth-mode
			    diary-mode
			    enwc-mode
			    calendar-mode
			    elfeed-search-mode
			    elfeed-show-mode
			    debugger-mode
			    Info-mode
			    ibuffer-sidebar-mode
			    ibuffer-mode
			    flycheck-error-list-mode
			    fireplace-mode
			    use-package-statistics-mode
			    dired-mode
			    dired-sidebar-mode
			    pdf-outline-buffer-mode
			    osm-mode
			    calc-mode
			    calc-trail-mode
			    special-mode
			    matlab-shell-mode
			    so-long-mode
			    apt-utils-mode
			    tmr-mode))
  :config
  (add-hook 'window-configuration-change-hook
	    #'(lambda ()
		(dolist (window (window-list))
		  (set-window-parameter window 'tab-line-cache nil))))
  (global-tab-line-mode +1))

(use-package centaur-tabs
  :disabled
  :straight t
  :after evil
  :bind ("s-/" . centaur-tabs-mode)
        (:map evil-normal-state-map
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
  :config (centaur-tabs-mode +1))

(use-package lg-centaur-tabs
  :load-path "~/.emacs.d/lisp/site-packages/"
  :after centaur-tabs)

;;; Scrolling tweaks and perfomance
(use-package emacs ; scrolling
  :init (pixel-scroll-precision-mode +1)
        (display-battery-mode +1)
  :custom (use-system-tooltips t)
	  (scroll-step 1)
	  (scroll-conservatively 100))

(use-package fast-scroll
  :straight t
  :diminish fast-scroll-mode
  :config (fast-scroll-config)
	  (fast-scroll-mode +1))

(use-package scroll-on-jump
  :demand t
  :straight (scroll-on-jump :type git
			    :host gitlab
			    :repo "ideasman42/emacs-scroll-on-jump")
  :after evil
  :config (scroll-on-jump-advice-add evil-undo)
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
  :custom (scroll-on-jump-smooth t)
          (scroll-on-jump-use-curve t)
          (scroll-on-jump-duration 0.2))

(use-package good-scroll
  :disabled t
  :straight t
  :config (good-scroll-mode +1))

;;; Lines
(use-package hl-line
  :straight nil
  :hook (prog-mode . hl-line-mode))

(use-package lin
  :straight (:host gitlab :repo "protesilaos/lin")
  :commands lin-mode
  :config (lin-add-to-many-modes))

(use-package hl-todo
  :straight t
  :config (global-hl-todo-mode +1))

;;; Misc
(use-package dashboard
  :disabled t
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
  :config (dashboard-setup-startup-hook))

(use-package outshine
  :straight t
  :hook (emacs-lisp-mode . outshine-mode)
  :diminish outshine-mode
  :general
  (general-def
    :keymaps 'outshine-mode-map
    :states 'normal
    [S-?\t] #'outshine-cycle
    [?\t] #'outshine-cycle))

(use-package outline
  :diminish outline-minor-mode
  :defer t)

(use-package display-line-numbers
  :hook ((prog-mode matlab-mode ledger-mode) . lg/display-line-numbers-mode-enable)
  :config
  (defun lg/display-line-numbers-mode-enable ()
    "Enable display-line-numbers"
    (interactive)
    (display-line-numbers-mode +1)
    (setq display-line-numbers 'relative)))

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
  :config (display-time-mode +1))

(use-package emacs 
  :custom (use-system-tooltips t))

(use-package git-gutter-fringe
  :diminish (global-git-gutter-mode git-gutter-mode)
  :straight t
  :custom (git-gutter:update-interval 0.2)
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(bottom nil))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(bottom nil))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil '(bottom nil))
  (global-git-gutter-mode +1))

(use-package visual-fill-column
  :defer t
  :straight t
  :custom (visual-fill-column-width 130)
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
      (visual-fill-column-mode +1))))
;; :hook
;; ((Info-mode
;;   org-mode
;;   minibuffer-mode
;;   ledger-mode) . lg/activate-visual-fill-center))

(use-package page-break-lines
  :straight t
  :diminish page-break-lines-mode
  :config (global-page-break-lines-mode +1))

(use-package eros
  :straight t
  :config (eros-mode +1))

(use-package sublimity
  :straight t)

(use-package rainbow-mode
  :straight t
  :diminish rainbow-mode
  :hook (prog-mode . rainbow-mode))

(use-package emojify
  :straight t
  :hook (erc-mode . emojify-mode)
  :commands emojify-mode)

(use-package beacon
  :disabled t ;; we favor pulsar
  :straight t
  :diminish beacon-mode
  :custom (beacon-size 20)
	  (beacon-blink-when-point-moves-vertically 1)
	  (beacon-blink-when-buffer-changes t)
	  (beacon-blink-when-window-changes t)
	  (beacon-blink-when-window-scrolls t)
	  (beacon-blink-when-focused t)
  :config (beacon-mode +1))

(use-package pulsar
  :straight t
  :custom (pulsar-face 'pulsar-red)
	  (pulsar-delay 0.05)
  :config (pulsar-global-mode +1))

(use-package lg-zen-mode)

(use-package logos
  :straight t
  :general
  (:keymaps 'outline-mode-map
	    :states 'normal
	    "C-j" #'logos-forward-page-dwim
	    "C-k" #'logos-backward-page-dwim)
  :custom
  (logos-hide-mode-line t)
  (logos-variable-pitch nil)
  (logos-outlines-are-pages t)
  (logos-outline-regexp-alist '((emacs-lisp-mode . "^;;; ")
				(org-mode . "^\\*+ +")
				(t . ";;[;]\\{1,8\\} ")))
  :config
  (let ((map global-map))
    (define-key map [remap narrow-to-region] #'logos-narrow-dwim)
    (define-key map [remap forward-page] #'logos-forward-page-dwim)
    (define-key map [remap backward-page] #'logos-backward-page-dwim)))

(provide 'lg-ui)
;;; lg-ui.el ends here
