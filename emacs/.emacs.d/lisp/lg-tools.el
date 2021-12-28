;; -*- lexical-binding: t; -*-
;;; lg-tools: tools for emacs, to do everything in emacs
;; Author: Lucas Gruss

;;; alert
(use-package alert
  :straight t
  :defer t
  :custom
  (alert-default-style 'notifications))

;;; Dired
(use-package dired
  :hook
  (dired-mode . dired-hide-details-mode)
  :custom
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-clean-confirm-killing-deleted-buffers nil)
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
  :bind (:map dired-sidebar-mode-map ("<localleader>m" . emms-play-dired))
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
  :custom
  (elfeed-feeds
   '(("https://xkcd.com/atom.xml" comic)
     ("https://protesilaos.com/codelog.xml" code)
     ("https://blog.tecosaur.com/tmio/rss.xml" org)
     ("https://www.youtube.com/feeds/videos.xml?channel_id=UC0uTPqBCFIpZxlz_Lv1tk_g" protesilaos)
     ("https://www.youtube.com/feeds/videos.xml?playlist_id=PL0H7ONNEUnnt59niYAZ07dFTi99u2L2n_" ouvrez_les_guillements usul)
     ("https://www.youtube.com/feeds/videos.xml?channel_id=UCKH_iLhhkTyt8Dk4dmeCQ9w" science)
     ("https://www.youtube.com/feeds/videos.xml?playlist_id=PL43OynbWaTMLEbdAWr-DnAfveOonmhlT1" france_inter)
     "https://api.lemediatv.fr/rss.xml"))
  :config
  (use-package lg-elfeed
    :load-path "~/.emacs.d/lisp/site-packages"
    :after elfeed))

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
  :defer t
  :commands (emms-play-dired)
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
  :general
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
  :disabled t
  :custom (espotify-service-name "spotifyd")
  :straight t)

;;; Pdf-tools
(use-package pdf-tools
  :straight t
  :hook
  (pdf-view-mode . (lambda () (blink-cursor-mode -1) (setq-local cursor-type nil)))
  (pdf-view-mode . pdf-isearch-minor-mode)
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
  :bind ("s-<return>" . 'lg/vterm-toggle)
  :commands (vterm-toggle)
  :custom
  (vterm-toggle-fullscreen-p nil)
  :init
  (defun lg/vterm-toggle ()
    (interactive)
    (if (string= (frame-parameter nil 'name) "yequake-vterm")
	(delete-frame (select-frame-by-name "yequake-vterm"))
      (vterm-toggle))))

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
  :defer 10
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
  :defer t
  :commands magit-status
  :ensure-system-package git)

;;;; Magit-todos
(use-package magit-todos
  :straight t
  :commands magit-status
  :defer t
  :config
  (magit-todos-mode +1))

;;;; Bluetooth
(use-package bluetooth
  :straight t
  :commands bluetooth-list-devices)

;;;; Pulseaudio
(use-package pulseaudio-control
  :straight t)

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
  :straight t
  :commands pass)

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
  :ensure-system-package ledger
  :straight t
  :defer t
  :magic ("\\.ledger$" . ledger-mode)
  :custom
  (ledger-schedule-file "~/Documents/factures/ledger/schedule.ledger")
  (ledger-post-amount-alignment-column 65)
  (ledger-reports 
   '(("bal" "%(binary) --real -f %(ledger-file) bal")
     ("reg" "%(binary) -f %(ledger-file) reg")
     ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
     ("account" "%(binary) -f %(ledger-file) reg %(account)")
     ("budget" "%(binary) --empty -S -T -f %(ledger-file) bal ^Budget:")))
  :config
  (add-hook 'ledger-mode-hook
	    (lambda ()
	      (setq-local tab-always-indent 'complete)
	      (setq-local completion-cycle-threshold t)
	      (setq-local ledger-complete-in-steps t)))
  (progn
    ;; org-cycle allows completion to work whereas outline-toggle-children does not
    (evil-define-key evil-normal-state-map evil-ledger-mode-map (kbd "TAB") #'org-cycle)
    (add-hook 'ledger-mode-hook #'outline-minor-mode)
    (font-lock-add-keywords 'ledger-mode outline-font-lock-keywords)))

;;; Scanner
(use-package scanner
  :ensure-system-package sane-utils
  :commands
  (scanner-scan-document
   scanner-scan-multi-doc
   scanner-scan-image
   scanner-scan-multi-images)
  :straight t)

;;; Zotero
;; this is meant to be used with an account, this is a wrapper to the web API
(use-package zotero
  :disabled t
  :straight t
  :commands (zotero-browser zotero-sync)
  :config
  (zotero-sync)
  (zotero-browser-sync))

;;; Zotxt
;; the zotxt plugin for zotero has to be installed
;; https://github.com/egh/zotxt
(use-package zotxt
  :straight t
  :defer t)

;;; App-launcher
(use-package app-launcher
  :straight (app-launcher :type git :host github
			  :repo "SebastienWae/app-launcher")
  :commands app-launcher-run-app
  :bind ("s-d" . 'app-launcher-run-app))

;;; wttrin
(use-package wttrin
  :straight t
  :commands wttrin
  :custom
  (wttrin-default-cities '("Paris" "Nantes" "Russange" ":help"))
  :config
  ;; https://github.com/bcbcarl/emacs-wttrin/issues/16
  (defun wttrin-fetch-raw-string (query)
    "Get the weather information based on your QUERY."
    (let ((url-user-agent "curl"))
      (add-to-list 'url-request-extra-headers wttrin-default-accept-language)
      (with-current-buffer
	  (url-retrieve-synchronously
	   (concat "http://fr.wttr.in/" query "?A")
	   (lambda (status) (switch-to-buffer (current-buffer))))
	(decode-coding-string (buffer-string) 'utf-8)))))

;;; Deamons
(use-package daemons
  :straight t)

;;; finito
;; (use-package finito
;;   :straight (finito :type git :repo "LaurenceWarne/finito.el")
;;   :config
;;   (finito-download-server-if-not-exists
;;    ;; Optional, but we can specify a callback to run when the server has
;;    ;; finished downloading, we choose here to start the server to override
;;    ;; the default lazy behaviour which starts the server whenever a finito
;;    ;; command is invoked
;;    (lambda () (finito-start-server-if-not-already))))

;;; browse-url
(use-package browse-url
  :straight nil
  :commands (browse-url-umpv browse-url-at-point-umpv)
  :config
  (defun browse-url-umpv (url &optional single)
    (start-process "mpv" nil (if single "mpv" "umpv")
		   (shell-quote-wildcard-pattern url)))

  (defun browse-url-at-point-umpv (&optional single)
    "Open link in mpv"
    (interactive "P")
    (let ((browse-url-browser-function
	   (if single
	       (lambda (url &optional _new-window) (browse-url-umpv url t))
	     #'browse-url-umpv)))
      (browse-url-at-point))))

;;; Simple-httpd
(use-package simple-httpd
  :straight t
  :defer t)

;;; Avy
(use-package avy
  :straight t)

;;; yasnippets
(use-package yasnippet
  :straight t
  :diminish yas-minor-mode
  :hook
  (org-mode . yas-minor-mode-on)
  (prog-mode . yas-minor-mode-on))

;;;; Yasnippet-snippets
(use-package yasnippet-snippets
  :straight t
  :after yasnippet)

;;;; Consult-yasnippet
(use-package consult-yasnippet
  :straight t
  :after yasnippet)

;;; yequake
(use-package yequake
  :straight t
  :defer t
  :init
  (defun lg/yequake-delete-run-app-frame ()
    (delete-frame (select-frame-by-name "yequake-run-app")))

  (defun yequake-run-app (&optional goto keys)
    (let* ((remove-hook-fn (lambda ()
			     (remove-hook 'minibuffer-exit-hook #'lg/yequake-delete-run-app-frame))))
      (add-hook 'minibuffer-exit-hook remove-hook-fn)
      (add-hook 'minibuffer-exit-hook #'lg/yequake-delete-run-app-frame)
      (app-launcher-run-app)))
  :custom
  (yequake-frames
   '(("org-capture" 
      (buffer-fns . (yequake-org-capture))
      (width . 0.75)
      (height . 0.5)
      (alpha . 0.95)
      (frame-parameters . ((undecorated . t)
                           (skip-taskbar . t)
                           (sticky . t))))
     ("yequake-vterm"
      (buffer-fns . (vterm
		     evil-insert-state))
      (width . 0.75)
      (height . 0.5)
      (top . 200)
      (frame-parameters . ((undecorated . t)
                           (skip-taskbar . t)
                           (sticky . t))))
     ("yequake-run-app"
      (buffer-fns . (app-launcher-run-app))
      (width . 0.75)
      (height . 0.5)
      (top . 200)
      (frame-parameters . ((undecorated . nil)
                           (skip-taskbar . t)
			   (minibuffer . only)
                           (sticky . t))))
     )))

;;; flyspell
(use-package flyspell
  :straight t
  :hook (org-mode . flyspell-mode))

;;; flyspell-correct
;; provide interface through completing-read
(use-package flyspell-correct
  :straight t
  :after flyspell)

;;; openwith
(use-package openwith
  :straight t
  :config
  (openwith-mode +1)
  (setq openwith-associations
   (list
    (list (openwith-make-extension-regexp
           '("m4a" "mpg" "mpeg" "mp3" "mp4"
             "avi" "wmv" "wav" "mov" "flv"
             "ogm" "ogg" "mkv"))
          "mpv"
          '(file))
    (list (openwith-make-extension-regexp
           '("doc" "xls" "ppt" "odt" "ods" "odg" "odp"))
          "libreoffice"
          '(file)))))

;;; dirvish
(use-package dirvish
  :straight t
  :ensure-system-package (exa)
  :commands (dirvish)
  :hook
  (dirvish-mode . (lambda () (all-the-icons-dired-mode -1))))

;;; vc
(use-package vc
  :custom
  (vc-follow-symlinks t))

;;; browse-url
(use-package browse-url
  :defer t
  :custom
  (browse-url-handlers
   '(("\\`mailto:" . browse-url--mailto)
    ("\\www\.youtube\.com" . browse-url-umpv)
    ("\\`man:" . browse-url--man)
    (browse-url--non-html-file-url-p . browse-url-emacs))))

(provide 'lg-tools)
