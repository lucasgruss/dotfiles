(use-package emacs
  :config
  (display-battery-mode -1)
  (setq display-time-format "%H:%M")
  (setq scroll-step 1)
  (setq scroll-conservatively 10000)
  (display-time-mode +1)
  (set-face-attribute 'default nil :family "Iosevka" :weight 'normal :height 110)
  (set-face-attribute 'fixed-pitch nil :family "Iosevka" :weight 'normal :height 110)
  (set-face-attribute 'variable-pitch nil :family "Roboto Mono" :height 110 :width 'normal)

  ;; themes
  (defvar lg/light-themes '(modus-operandi) "List of light themes.")
  (defvar lg/dark-themes '(modus-vivendi doom-one doom-dark+) "List of dark themes.")

  (defun lg/theme-propagate (theme &rest args)
    "Apply system wide settings that are consistent with the
modus themes. A suitable gtk and icon themes are applied."
    (when (member theme lg/light-themes)
      (shell-command "sed -i 's/ThemeName \"Adwaita-dark\"/ThemeName \"Adwaita\"/g' ~/.xsettingsd")
      (shell-command "sed -i 's/IconThemeName \"Papirus-Dark\"/IconThemeName \"Papirus-Light\"/g' ~/.xsettingsd"))
    (when (member theme lg/dark-themes)
      (shell-command "sed -i 's/ThemeName \"Adwaita\"/ThemeName \"Adwaita-dark\"/g' ~/.xsettingsd")
      (shell-command "sed -i 's/IconThemeName \"Papirus-Light\"/IconThemeName \"Papirus-Dark\"/g' ~/.xsettingsd"))
    (shell-command "killall -HUP xsettingsd")
    (shell-command "xrdb ~/.Xresources"))

  (defun load-theme--disable-old-theme(theme &rest args)
    "Disable current theme before loading new one."
    (mapcar #'disable-theme custom-enabled-themes))

  (advice-add 'load-theme :after #'lg/theme-propagate '(depth 100))
  (advice-add 'load-theme :before #'load-theme--disable-old-theme))


(use-package moody
  :straight t
  :demand t
  :hook (after-init . lg/force-moody)
  :config
  (setq moody-mode-line-height 20)
  (setq x-underline-at-descent-line t)
  ;; modeline
  ;; https://emacs.stackexchange.com/questions/5529/how-to-right-align-some-items-in-the-modeline
  ;; write a function to do the spacing

  (defvar mode-line-format-left
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
      (vc-mode moody-vc-mode))
    "Content of the left of the modeline")

  (defvar mode-line-format-right
    '("%e"
      mode-line-modes
      (moody-tab mode-line-misc-info)
      mode-line-end-spaces)
    "Content of the right of the modeline")

  (defun simple-mode-line-render (left right)
    "Return a string of `window-width' length containing LEFT, and RIGHT
 aligned respectively."
    (let* ((available-width (- (window-width) (length left) 6)))
      (format (format " %%s %%%ds " available-width) left right)))

  (defun lg/force-moody ()
    "Redisplay moody after loading emacs."
    (setq-default mode-line-format
	  '((:eval (simple-mode-line-render
		    (format-mode-line mode-line-format-left)
		    (format-mode-line mode-line-format-right))))))
  (lg/force-moody))
					;(moody-replace-mode-line-buffer-identification)
					;(moody-replace-vc-mode))

(use-package dashboard
  :straight t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-set-init-info t)
  (setq dashboard-center-content t)
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-navigator t))

(use-package helpful
  :straight t
  :commands (helpful-callable
	     helpful-key
	     helpful-symbol
	     helpful-variable)
  :general
  (my-leader-def
   :prefix "SPC" :states 'normal :keymaps 'override
   "hf" 'helpful-callable
   "hk" 'helpful-key
   "ho" 'helpful-symbol
   "hv" 'helpful-variable))

(use-package outshine
  :hook (emacs-lisp-mode . outshine-mode)
  :diminish outshine-mode
  :general
  (general-def :keymaps 'outshine-mode-map :states 'normal
    [S-?\t] #'outshine-cycle)
  :straight t)

(use-package all-the-icons
  :straight t
  :config
  (add-to-list 'all-the-icons-mode-icon-alist
               '(exwm-mode  all-the-icons-faicon "toggle-on" :height 1.0 :v-adjust -0.2
                            :face all-the-icons-green))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.m$" all-the-icons-fileicon "matlab" :face all-the-icons-orange)))

(use-package all-the-icons-dired
  :straight t
  :after (dired all-the-icons)
  :diminish all-the-icons-dired-mode
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package modus-themes
  :demand
  :straight t
  :config
  (setq modus-themes-slanted-constructs t)
  (setq modus-themes-bold-constructs nil)
  (setq modus-themes-fringes 'subtle) ; {nil,'subtle,'intense})
  ;; Options for `modus-themes-lang-checkers': nil,
  ;; 'straight-underline, 'subtle-foreground,
  ;; 'subtle-foreground-straight-underline, 'intense-foreground,
  ;; 'intense-foreground-straight-underline, 'colored-background
  (setq modus-themes-lang-checkers 'straight-underline)

  ;; Options for `modus-themes-mode-line': nil, '3d, 'moody,
  ;; 'borderless, 'borderless-3d, 'borderless-moody, 'accented,
  ;; 'accented-3d, 'accented-moody
  (setq modus-themes-mode-line 'accented-moody)

  ;; Options for `modus-themes-syntax': nil, 'faint,
  ;; 'yellow-comments, 'green-strings,
  ;; 'yellow-comments-green-strings, 'alt-syntax,
  ;; 'alt-syntax-yellow-comments, 'faint-yellow-comments
  (setq modus-themes-syntax nil)

  ;; Options for `modus-themes-hl-line': nil, 'intense-background,
  ;; 'accented-background, 'underline-neutral,
  ;; 'underline-accented, 'underline-only-neutral,
  ;; 'underline-only-accented
  (setq modus-themes-intense-hl-line 'accented-background)
  (setq modus-themes-subtle-line-numbers nil)
  (setq modus-themes-paren-match 'subtle-bold) ; {nil,'subtle-bold,'intense,'intense-bold}

  ;; Options for `modus-themes-links': nil, 'faint,
  ;; 'neutral-underline, 'faint-neutral-underline, 'no-underline,
  ;; 'underline-only, 'neutral-underline-only
  (setq modus-themes-links 'neutral-underline)

  ;; Options for `modus-themes-prompts': nil, 'subtle-accented,
  ;; 'intense-accented, 'subtle-gray, 'intense-gray
  (setq modus-themes-prompts 'intense-gray)
  (setq modus-themes-completions 'moderate) ; {nil,'moderate,'opinionated})

  ;; Options for `modus-themes-region': nil, 'no-extend, 'bg-only,
  ;; 'bg-only-no-extend, 'accent, 'accent-no-extend
  (setq modus-themes-region 'accent)

  ;; Options for `modus-themes-diffs': nil, 'desaturated,
  ;; 'fg-only, 'bg-only, 'deuteranopia,
  (setq modus-themes-diffs 'deuteranopia) ;
  (setq modus-themes-org-blocks 'rainbow) ; {nil,'greyscale,'rainbow}
  (setq modus-themes-org-habit nil) ; {nil,'simplified,'traffic-light}
  (setq modus-themes-headings '((t . highlight)))
  (setq modus-themes-variable-pitch-ui nil)
  (setq modus-themes-variable-pitch-headings t)
  (setq modus-themes-scale-headings t)
  (setq modus-themes-scale-1 1.1)
  (setq modus-themes-scale-2 1.15)
  (setq modus-themes-scale-3 1.21)
  (setq modus-themes-scale-4 1.27)
  (setq modus-themes-scale-5 1.33))

(use-package modus-themes-exporter
  :load-path "~/.emacs.d/lisp/"
  :after modus-themes
  :demand t
  :ensure-system-package xsettingsd
  :config
  (defun lg/modus-theme-propagate (theme &rest args)
    "Xresources are computed based on the modus-themes-exporter
package and the programs that use the Xresources have the
settings applied to them."
    (when (member theme '(modus-operandi modus-vivendi))
      (modus-themes-exporter-export "xcolors" "~/.Xresources")))
  (advice-add #'load-theme :after #'lg/modus-theme-propagate)
  (load-theme 'modus-operandi t nil))

(use-package doom-themes
  :commands (load-theme)
  :straight t)

(use-package centaur-tabs
  :defer 2
  :straight t
  :hook
  (org-ql-sidebar-buffer-setup . centaur-tabs-local-mode)
  (dashboard-mode . centaur-tabs-local-mode)
  (calendar-mode . centaur-tabs-local-mode)
  (ibuffer-sidebar-mode . centaur-tabs-local-mode)
  (ibuffer-mode . centaur-tabs-local-mode)
  (dired-sidebar-mode . centaur-tabs-local-mode)
  (dired-mode . centaur-tabs-local-mode)
  (pdf-outline-buffer-mode . centaur-tabs-local-mode)
  (exwm-floating-setup . centaur-tabs-local-mode)
  (calc-mode . centaur-tabs-local-mode)
  (calc-trail-mode . centaur-tabs-local-mode)
  :config
  (setq centaur-tabs-style "bar")
  (setq centaur-tabs-set-modified-marker t)
  (setq centaur-tabs-set-icons t)
  (setq centaur-tabs-gray-out-icons t)
  (setq centaur-tabs-set-bar 'under)
  (setq centaur-tabs-show-navigation-buttons t)
  (setq centaur-tabs-show-new-tab-button t)
  (setq centaur-tabs-height 21)
  (setq centaur-tabs-enable-ido-completion nil)
  (setq centaur-tabs-cycle-scope 'tabs)
  (setq centaur-tabs-plain-icons nil)
  (setq centaur-tabs-label-fixed-length 15)
  (setq uniquify-separator "/")
  (centaur-tabs-mode +1)
  (use-package lg-centaur-tabs
    :load-path "~/.emacs.d/lisp/"))

(use-package display-line-numbers
  :hook (prog-mode . lg/display-line-numbers-mode-enable)
  :config
  (defun lg/display-line-numbers-mode-enable ()
    "Enable display-line-numbers"
    (display-line-numbers-mode +1)
    (setq display-line-numbers 'relative)))

;;; * Scrolling performances

(use-package fast-scroll
  :disabled t
  :straight t
  :config (fast-scroll-mode +1))

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
  (setq scroll-on-jump-smooth t)
  (setq scroll-on-jump-use-curve t)
  (setq scroll-on-jump-duration 0.1))

(use-package good-scroll
  :disabled t
  :straight t
  :config
  (good-scroll-mode +1))

(use-package hl-line
  :straight nil
  :config
  (global-hl-line-mode +1))
  
(use-package git-gutter-fringe
  :straight t
  :config
  (global-git-gutter-mode +1))

(provide 'lg-ui)
