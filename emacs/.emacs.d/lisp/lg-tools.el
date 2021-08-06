;;; lg-tools: tools for emacs, to do everything in emacs
;; Author: Lucas Gruss

;;; Dired
(use-package dired
  :hook (dired-mode . dired-hide-details-mode)
  :custom
  (dired-listing-switches "-al --group-directories-first")
  :config
  (setq dired-compress-directory-default-suffix ".zip")
  (general-def :keymaps 'dired-mode-map :states 'normal
    "h" #'dired-up-directory
    "l" #'dired-find-file))

;;; Diredfl
(use-package diredfl
  :straight t
  :after dired
  :hook (dired-mode . diredfl-mode))

;;; Dired-sidebar
(use-package dired-sidebar
  :straight t
  :after dired
  :commands (dired-sidebar-find-file dired-sidebar-toggle)
  :general
  (general-def :keymaps 'dired-sidebar-mode-map :states 'normal
    "h" #'dired-sidebar-up-directory
    "l" #'dired-sidebar-find-file)
  :hook
  (dired-sidebar-mode . (lambda () (setq-local header-line-format "Dired-sidebar")))
  :custom
  (dired-sidebar-width 30)
  (dired-sidebar-no-delete-other-windows t))

;;; Dired-hide-dotfiles
(use-package dired-hide-dotfiles
  :after dired
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :straight t)

;;; Elfeed
(use-package elfeed
  :straight t
  :commands (elfeed elfeed-update)
  :config
  (use-package lg-elfeed
    :load-path "~/.emacs.d/lisp/site-packages")
  (setq elfeed-feeds
	'(("https://xkcd.com/atom.xml" comic)
	  ("https://protesilaos.com/codelog.xml" code)
	  ("https://blog.tecosaur.com/tmio/rss.xml" org)
	  ("https://www.youtube.com/feeds/videos.xml?channel_id=UC0uTPqBCFIpZxlz_Lv1tk_g" protesilaos)
	  ("https://www.youtube.com/feeds/videos.xml?playlist_id=PL0H7ONNEUnnt59niYAZ07dFTi99u2L2n_" ouvrez_les_guillements usul)
	  ("https://www.youtube.com/feeds/videos.xml?channel_id=UCKH_iLhhkTyt8Dk4dmeCQ9w" science)
	  ("https://www.youtube.com/feeds/videos.xml?playlist_id=PL43OynbWaTMLEbdAWr-DnAfveOonmhlT1" france_inter)
	  "https://api.lemediatv.fr/rss.xml")))
  ;; :bind ((:map elfeed-search-mode-map
  ;;         ("v" . #'elfeed-view-mpv)
  ;;         ("a" . #'elfeed-listen-mpv))
  ;;        (:map elfeed-show-mode-map
  ;;         ("v" . #'elfeed-view-mpv)
  ;;         ("a" . #'elfeed-listen-mpv)))

;;; Elfeed-org
(use-package elfeed-org
  :disabled t
  :after elfeed
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files "~/org/elfeed.org"))

;;; Multimedia
;;;; Emms
(use-package emms
  :straight t
  :ensure-system-package (exiftool
			  (mp3info . mp3info)
			  (mpv . mpv)
			  (mplayer . mplayer))
  :defer 5
  :custom
  (emms-source-file-default-directory "~/Audio/Musique/")
  (emms-streams-file "~/.config/doom/emms/streams.emms")
  (emms-playlist-buffer-name "*Music*")
  (emms-info-asynchronously t) ; update tags asynchronously)
  (emms-info-functions '(emms-info-exiftool))
  (emms-source-file-directory-tree-function 'emms-source-file-directory-tree-find)
  (emms-player-list '(emms-player-mplayer emms-player-mpv))
  (emms-player-mpv-parameters '("--quiet" "--really-quiet" "--no-audio-display"))
  (emms-info-functions '(emms-info-mp3info emms-info-cueinfo))
  :init
  (defun lg/find-music-directory ()
    (interactive)
    ;;(find-file "~/Audio/Musique")
    (let ((default-directory "~/Audio/Musique"))
      (dired-sidebar-toggle-sidebar)))
  :general
  (my-local-leader-def
    :keymaps 'dired-sidebar-mode-map
    "" nil
    "m" #'emms-play-dired)
  (general-def :states 'normal :keymaps 'emms-playlist-mode-map
    "q" #'emms-playlist-mode-bury-buffer)

  :config
  (emms-all)

  (defun lg/emms-kill-mpv ()
    (interactive)
    (shell-command "killall mpv"))

  (defcustom emms-sidebar-no-delete-other-windows t
    "Whether or not to add no-delete-other-window parameter to window.")

  (defcustom lg/emms-sidebar-display-alist '((side . left)
					     (slot . -2))
    "Parameters for the display of the emms-sidebar.")

  (defun lg/emms-go-playlist ()
    "Go to the playlist buffer. This function exist for the sake of
the display-buffer-alist variable and enable the display of the
playlist in a side-window"
    (interactive)
    (display-buffer-in-side-window (get-buffer-create "*Music*")
				   lg/emms-sidebar-display-alist)
    (let ((window (get-buffer-window (get-buffer "*Music*"))))
      (when emms-sidebar-no-delete-other-windows
	(set-window-parameter window 'no-delete-other-windows t))
      (set-window-dedicated-p window t)
      (with-selected-window window
	(let ((window-size-fixed))
	  (dired-sidebar-set-width dired-sidebar-width)))))

  (add-hook 'emms-player-stopped-hook #'lg/emms-kill-mpv))

;;;; Smudge (spotify)
(use-package smudge
  :ensure-system-package curl
  :straight (smudge :host github :repo "danielfm/smudge")
  :commands
  (smudge-my-playlists
   smudge-track-search
   smudge-playlist-search)
  :custom
  (httpd-port 8081)
  (smudge-oauth2-callback-port "8081")
  (smudge-transport 'connect))

;;;; Espotify
(use-package espotify
  :commands (espotify-next espotify-previous espotify-play-pause)
  :straight t)

;;; Pdf-tools
(use-package pdf-tools
  :straight t
  :hook (pdf-view-mode . (lambda () (blink-cursor-mode -1)
			   (setq-local cursor-type nil)))
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :config

  (defun lg/pdf-tools-midnight-colors-with-theme (theme &rest args)
    "Use the loaded theme background and foreground colors for the
    midnight mode."
    (setq pdf-view-midnight-colors
	  `(,(face-foreground 'default) . ,(face-background 'default))))

  (advice-add 'load-theme :after #'lg/pdf-tools-midnight-colors-with-theme))

;;; Vterm
(use-package vterm
  ;; :ensure-system-package (libvterm . libvterm-dev)
  :straight (vterm :type git :repo "akermu/emacs-libvterm")
  :commands vterm
  :custom
  (vterm-shell "bash")
  (vterm-always-compile-module t)
  (vterm-module-cmake-args "-DUSE_SYSTEM_VTERM=YES"))

;;; Multi-vterm
(use-package multi-vterm
  :straight t
  :after vterm)

;;; Vterm-toggle
;; run-or-raise-or-dismiss for vterm
(use-package vterm-toggle
  :straight t
  :after vterm
  :bind ("s-<return>" . 'vterm-toggle)
  :custom
  (vterm-toggle-fullscreen-p nil))

;;; ibuffer
(use-package ibuffer
  :commands ibuffer
  :custom (ibuffer-use-header-line nil))

;;; ibuffer-sidebar
(use-package ibuffer-sidebar
  :straight t
  :commands ibuffer-sidebar-toggle-sidebar
  :hook
  (ibuffer-sidebar-mode . (lambda ()
			    ;; avoid ibuffer hijacking the header-line
			    (setq-local ibuffer-use-header-line nil)
			    (setq-local ibuffer-header-line-format "Buffers")
			    (setq header-line-format "Buffers")))
  :custom
  (ibuffer-sidebar-width 30))
  ;;(setq ibuffer-sidebar-mode-line-format nil))

;;; Integration with external tools
;;;; exec-path-from-shell
(use-package exec-path-from-shell
  :straight t
  :config
  (exec-path-from-shell-initialize))

;;;; Ripgrep
(use-package rg
  :commands rg
  :straight t
  :ensure-system-package rg
  :custom (rg-executable "/usr/bin/rg"))

;;;; Magit
(use-package magit
  :straight t
  :ensure-system-package git
  :commands magit-status)

;;;; Magit-todos
(use-package magit-todos
  :straight t
  :after magit
  :config
  (magit-todos-mode +1))

;;;; Bluetooth
(use-package bluetooth
  :straight t
  :commands bluetooth-list-devices)

;;;; Disk-usage
(use-package disk-usage
  :straight t
  :commands disk-usage)

;;;; Password-store: integration of pass into emacs
(use-package password-store
  :straight t
  :commands (password-store-copy password-store-insert))

;;;; Pass: frontend to pass in Emacs
(use-package pass
  :straight t)

;;; EWW
(use-package eww
  :commands (eww eww-browse-with-history)
  :hook (eww-after-render . prot-eww--rename-buffer)
  ;; :init
  ;; (map! (:leader
  ;;        :prefix ("o" . "open")
  ;;        :desc "eww" "w" #'eww-browse-with-history)
  ;;       (:map eww-link-keymap
  ;;        ;"v" #'eww-mpv-video-at-point
  ;;        "a" #'eww-mpv-audio-at-point
  ;;        "C-j" #'eww-next-url
  ;;        "C-k" #'eww-previous-url))
  :custom
  (eww-download-directory "~/Téléchargements/eww/")
  (eww-desktop-data-save '(:url :title))
  :config
  (advice-add 'eww-back-url :after #'prot-eww--rename-buffer)
  (advice-add 'eww-forward-url :after #'prot-eww--rename-buffer)
  (use-package lg-eww
    :load-path "lisp/site-packages"))

;;; Eshell
(use-package eshell-info-banner
  :commands eshell
  :straight (eshell-info-banner :type git
                                :host github
                                :repo "phundrak/eshell-info-banner.el")
  :hook (eshell-banner-load . eshell-info-banner-update-banner))

;;; Ledger mode
(use-package ledger-mode
  :straight t
  :defer t)

;;; Scanner
(use-package scanner
  :ensure-system-package sane-utils
  :commands
  (scanner-scan-document
   scanner-scan-multi-doc
   scanner-scan-image
   scanner-scan-multi-images)
  :straight t)

(provide 'lg-tools)
