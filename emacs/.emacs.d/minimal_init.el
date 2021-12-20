;;; init.el --- Minimalist configuration for emacs on Windows in <200 locs
;; Author:
;;   Lucas GRUSS
;; Description:
;;   Config kept minimal on purpose as I use it on my work machine. As
;;   I don't have admin rights on the machine, packages have to be
;;   manually managed.

;;; ui tweaks
(set-face-attribute 'default nil :family "Iosevka" :weight 'normal :height 110)
(tool-bar-mode -1)
(menu-bar-mode -1) 
(scroll-bar-mode -1) 
(tab-bar-mode +1)
(global-tab-line-mode -1)
(setq visible-bell t)
(setq inhibit-startup-screen t) 

;;; some convenient tweaks
(fset 'yes-or-no-p 'y-or-n-p)
(setq make-backup-files nil)
(setq scroll-conservatively 100)

;;; My own set of variables and functions
(defvar lg/emacs-packages-directory (expand-file-name "packages" user-emacs-directory) 
  "Path to the directory containing all the packages")

(defvar lg/packages-paths
  '("use-package" "annalist.el" "emacs-which-key"
    "evil" "evil-anzu" "evil-collection" "undo-tree" "queue"
    "framemove" "modus-themes" "super-save"
    "corfu" "consult" "marginalia" "selectrum" "orderless"
    "magit/lisp" "transient/lisp" "with-editor" "dash")
  "Name of the packages to be added to the path")

(load (setq custom-file (expand-file-name "custom.el" user-emacs-directory)))

(defun lg/go-init () (interactive) (find-file (expand-file-name "init.el" user-emacs-directory)))
(defun lg/dwim-switch-tab (arg)
  "Switch tabs. Priority is given to tab-line tabs. If the mode
is not active, try switching tab-bar tabs"
  (if tab-line-mode
      (if arg (tab-line-switch-to-next-tab)
	(tab-line-switch-to-prev-tab))
    (if tab-bar-mode
	(if arg (tab-bar-switch-to-next-tab)
	  (tab-bar-switch-to-prev-tab)))))
(defun lg/dwim-switch-to-next-tab () (interactive) (lg/dwim-switch-tab t))
(defun lg/dwim-switch-to-prev-tab () (interactive) (lg/dwim-switch-tab nil))

;;; Packages
(dolist (path lg/packages-paths)
  (add-to-list 'load-path (expand-file-name path lg/emacs-packages-directory)))

(require 'use-package)
(use-package paren :config (show-paren-mode +1))

(use-package recentf
  :custom (recentf-max-saved-items 1000)
  :config (recentf-mode +1))

(use-package super-save :config (super-save-mode +1))

(use-package display-line-numbers
  :custom (display-line-numbers 'relative)
  :config (global-display-line-numbers-mode +1))

(use-package evil
  :demand t
  :init
  (setq evil-undo-system 'undo-tree)
  (setq evil-want-keybinding nil)
  (setq evil-want-Y-yank-to-eol t)
  (defvar lg/leader-map (make-sparse-keymap) "Keymap for \"leader key\" shortcuts.")
  :bind (:map lg/leader-map
	      ("SPC" . execute-extended-command)
	      ("o" . other-window)
	      ("m" . evil-send-localleader)
	      ("bl" . list-bookmarks)
	      ("bj" . bookmark-jump)
	      ("bm" . bookmark-set)
	      ("cc" . comment-line)
	      ("ca" . comment-dwim)
	      ("fs" . save-buffer)
	      ("ff" . find-file)
	      ("fr" . consult-recent-file)
	      ("fo" . find-file-other-window)
	      ("fe" . lg/go-init)
	      ("ht" . consult-theme)
	      ("hi" . info)
	      ("hv" . describe-variable)
	      ("hf" . describe-function)
	      ("hk" . describe-key)
	      ("tf" . toggle-frame-fullscreen)
	      ("wo" . delete-other-windows)
	      ("ws" . evil-window-split)
	      ("wv" . evil-window-vsplit)
	      ("wq" . delete-window)
	      :map evil-normal-state-map
	      ("gt" . lg/dwim-switch-to-next-tab)
	      ("gT" . lg/dwim-switch-to-prev-tab)
	      :map evil-visual-state-map
	      ("gr" . eval-region))
  :config
  (evil-define-key evil-motion-state-map 'global-map (kbd "C-e") 'move-end-of-line)
  (define-key evil-normal-state-map (kbd "<SPC>") lg/leader-map)
  (define-key evil-visual-state-map (kbd "<SPC>") lg/leader-map)
  (evil-mode +1))

(use-package evil-collection :after evil :config (evil-collection-init))
(use-package evil-anzu :after evil :config (global-anzu-mode +1))
(use-package undo-tree :config (global-undo-tree-mode +1))
(use-package which-key :config (which-key-mode +1))
(use-package marginalia :config (marginalia-mode +1))
(use-package orderless :custom (completion-styles '(orderless)))
(use-package corfu :custom (corfu-auto t) :config (corfu-global-mode +1))
(use-package selectrum
  :demand t
  :bind (:map selectrum-minibuffer-map
	      (("C-j" . selectrum-next-candidate)
	       ("<wheel-down>" . selectrum-next-candidate)
	       ("C-k" . selectrum-previous-candidate)
	       ("<wheel-up>" . selectrum-previous-candidate)
	       ("C-l" . selectrum-insert-current-candidate)
	       ("<escape>" . keyboard-quit)))
  :config 
  (selectrum-mode +1))

(use-package consult
  :custom (consult-preview-key nil)
  :bind (:map lg/leader-map
	      (("bb" . 'consult-buffer)
	       ("ss" . 'consult-line))))

(use-package org-indent :hook (org-mode . org-indent-mode) :after org)
(use-package org
  :bind (:map org-mode-map ("<localleader>t" . org-todo))
  :custom
  (org-agenda-files '("c:/Users/lgruss/Documents/Thèse/org/todo.org"))
  (org-fontify-whole-block-delemiter-line nil))

(use-package dired
  :custom (dired-auto-revert-buffer t)
  :hook (dired-mode . dired-hide-details-mode)
  :config
  (evil-define-key 'normal dired-mode-map (kbd "<SPC>") lg/leader-map)
  (evil-define-key 'normal dired-mode-map "h" 'dired-up-directory)
  (evil-define-key 'normal dired-mode-map "l" 'dired-find-file))

(use-package windmove
  :bind (:map lg/leader-map
	      (("wh" . 'windmove-left)
	       ("wj" . 'windmove-down)
	       ("wk" . 'windmove-up)
	       ("wl" . 'windmove-right))))

(use-package winner
  :bind (:map lg/leader-map
	      (("wu" . 'winner-undo)
	       ("wr" . 'winner-redo)))
  :config (winner-mode +1))

(use-package framemove :config (setq framemove-hook-into-windmove t))

(use-package modus-themes
  :custom
  (modus-themes-variable-pitch-headings nil)
  (modus-themes-headings '(no-bold rainbow background))
  :config (load-theme 'modus-operandi))

(use-package magit
  :config
  ;; WORKAROUND https://github.com/magit/magit/issues/2395
  (define-derived-mode magit-staging-mode magit-status-mode "Magit staging"
    "Mode for showing staged and unstaged changes."
    :group 'magit-status)
  (defun magit-staging-refresh-buffer ()
    (magit-insert-section (status)
      (magit-insert-untracked-files)
      (magit-insert-unstaged-changes)
      (magit-insert-staged-changes)))
  (defun magit-staging ()
    (interactive)
    (magit-mode-setup #'magit-staging-mode)))

(use-package prog-mode
  :hook (prog-mode . prettify-symbols-mode)
  :config (setq-default prettify-symbols-alist
			'(("alpha" . 945)
			  ("beta" . 946)
			  ("gamma" . 947)
			  ("delta" . 948)
			  ("epsilon" . 949)
			  ("lambda" . 955))))
