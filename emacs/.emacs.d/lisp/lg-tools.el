;;; lg-tools.el : tools for emacs, to do everything in emacs

(use-package pdf-tools
  :straight t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :config
  (setq pdf-view-midnight-colors '("#ffffff" . "#000000")))

(use-package vterm
  :straight t
  :bind ("s-<return>" . 'vterm)
  :init
  (setq vterm-shell "bash")
  (setq vterm-always-compile-module t)
  (setq vterm-module-cmake-args "-DUSE_SYSTEM_VTERM=YES"))

(use-package ibuffer)
(use-package ibuffer-sidebar :straight t)

;; DIRED
(use-package dired
  :defer t
  :hook (dired-mode . dired-hide-details-mode)
  :config
  (setq dired-listing-switches "-al --group-directories-first")
  (general-def :keymaps 'dired-mode-map :states 'normal
    "h" #'dired-up-directory
    "l" #'dired-find-file))

(use-package wdired)

(use-package diredfl
  :straight t
  :hook (dired-mode . diredfl-mode))

(use-package dired-sidebar
  :straight t
  :general
  (general-def :keymaps 'dired-mode-map :states 'normal
    "h" #'dired-sidebar-up-directory
    "l" #'dired-sidebar-find-file))

(use-package elfeed
  :straight t
  :config
  (use-package lg-elfeed)
  ;; :bind ((:map elfeed-search-mode-map
  ;;         ("v" . #'elfeed-view-mpv)
  ;;         ("a" . #'elfeed-listen-mpv))
  ;;        (:map elfeed-show-mode-map
  ;;         ("v" . #'elfeed-view-mpv)
  ;;         ("a" . #'elfeed-listen-mpv)))
  :general
  (my-leader-def
   :prefix "SPC" :states 'normal
   "o" '(:ignore t :which-key "Open")
   "of" '(elfeed :which-key "Open elfeed")
   "oF" '(elfeed-update :which-key "update elfeed")))

(use-package elfeed-org
  :disabled t
  :straight t
  :after elfeed
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files "~/org/elfeed.org"))

(use-package emms
  :straight t
  :ensure-system-package ((mp3info . mp3info)
			  (mpv . mpv)
			  (mplayer . mplayer))
  :init
  (setq emms-source-file-default-directory "~/Audio/Musique/")
  (setq emms-streams-file "~/.config/doom/emms/streams.emms")
  (setq emms-playlist-buffer-name "*Music*")
  (setq emms-info-asynchronously t) ; update tags asynchronously)
  (setq emms-source-file-directory-tree-function 'emms-source-file-directory-tree-find)
  ;;emms-player-list '(emms-player-mpg321 emms-player-ogg123 emms-player-mplayer-playlist emms-player-mplayer emms-player-mpv emms-player-vlc emms-player-vlc-playlist)
  (setq emms-player-list '(emms-player-mplayer emms-player-mpv))
					;(setq emms-mode-line-mode-line-function nil)
  (setq emms-player-mpv-parameters '("--quiet" "--really-quiet" "--no-audio-display"))

  (defun lg/find-music-directory ()
    (interactive)
    ;;(find-file "~/Audio/Musique")
    (let ((default-directory "~/Audio/Musique"))
      (dired-sidebar-toggle-sidebar)))
  :general
  ;; (general-define-key :states 'normal
  ;;   :keymaps 'emms-playlist-mode-map
  ;; 	  "q" #'emms-playlist-mode-bury-buffer)
  ;; (my-local-leader-def
  ;;   :keymaps 'dired-mode-map
  ;;   "m" 'emms-play-dired
  ;;   "M" 'emms-add-dired)
  (my-leader-def
    :prefix "SPC" :states 'normal :keymaps 'override
    "e" '(:ignore t :which-key "emms")
    "ej" #'emms-next
    "ek" #'emms-previous
    "es" #'emms-stop
    "ee" #'emms-pause
    "eS" #'emms-shuffle
    "ea" #'emms-show-all
    "eb" #'lg/find-music-directory
    "em" #'emms
    "er" #'emms-streams)
  (my-local-leader-def
    :states 'normal :keymaps '(dired-mode-map override)
    "" nil
    "m" #'emms-play-dired)
  :config
  (emms-all)
  (defun lg/emms-kill-mpv ()
    (interactive)
    (shell-command "killall mpv"))
  (setq emms-info-functions '(emms-info-mp3info emms-info-cueinfo))
  (add-hook 'emms-player-stopped-hook #'lg/emms-kill-mpv))

(use-package rg :commands rg :straight t
  :ensure-system-package rg
  :config
  (setq rg-executable "/usr/bin/rg"))

(use-package magit
  :straight t
  :ensure-system-package git
  :general
  (my-leader-def
    :prefix "SPC" :states '(normal emacs hybrid visual)
    "gg" #'magit-status))

(provide 'lg-tools)
