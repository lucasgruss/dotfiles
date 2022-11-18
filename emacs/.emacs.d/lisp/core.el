;;; core.el --- core packages needed for my configuration -*- lexical-binding: t; -*-
;; Author: Lucas Gruss
;; This file is NOT part of GNU Emacs.
;;
;;; Commentary:
;; This is the core of my emacs configuration. It covers the configuration of
;; use-package, the package manager and performance enhancements.
;;
;;; Code:

(setq native-comp-async-report-warnings-errors nil)
(setq native-comp-async-jobs-number 4)
(setq tls-checktrust t)
(setq gnutls-verify-error t)

;; magically bootstrap straight
(defvar bootstrap-version)
(setq straight-repository-branch "develop")
(setq straight-check-for-modifications '(check-on-save find-when-checking))
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-disable-native-compile nil)
(setq straight-disable-compile nil)

(straight-use-package 'use-package)

(use-package use-package
  :straight t
  :custom (use-package-compute-statistics t)
          (use-package-verbose nil)
  :config ;; add keywords
  (use-package bind-key :straight t) 
  (use-package use-package-ensure-system-package :straight t)
  (use-package diminish :straight t :config (diminish 'auto-fill-function)))

(use-package straight-x :straight nil) ; extended straight features

(use-package gcmh ;; Garbage collector magic hack
  :straight t
  :diminish gcmh-mode
  :config (gcmh-mode +1))

(use-package no-littering ;; remove pesky files polluting our config directory
  :straight t
  :after recentf
  :config
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))

(use-package esup ;; emacs startup time profiler
  :straight t
  :commands esup)

;; built-in package manager, useful to browse package repositories event if I
;; don't use it to install packages
(use-package package
  :straight nil
  :defer t
  :commands (describe-package)
  :config
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  (add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(use-package super-save ;; automatic saves
  :straight t
  :diminish super-save-mode
  :config (super-save-mode +1))

(use-package recentf ;; access recent files
  :custom (recentf-max-saved-items 1000)
  :defer 5
  :config (recentf-mode +1))

(use-package emacs ;; core and convienence settings
  :straight nil
  :diminish (auto-revert-mode eldoc-mode)
  :bind ("s-<escape>" . 'lg/kill-this-buffer)
	("s-b" . 'bury-buffer)
  :custom
  (idle-update-delay 0.3)
  (delete-by-moving-to-trash t "Safer to move in trash than be sorry.") 
  (comp-async-report-warnings-errors nil)
  (make-backup-files nil)
  (ring-bell-function 'ignore)
  (x-select-enable-primary t)
  (select-enable-clipboard t)
  (inhibit-startup-screen t)
  (y-or-n-p-use-read-key t)
  (x-select-enable-clipboard-manager t)
  (mouse-scroll-delay 0.01)
  (mouse-drag-and-drop-region-cross-program t)
  :config
  (global-so-long-mode +1)
  (context-menu-mode +1)
  (show-paren-mode +1)
  (savehist-mode +1)
  (global-auto-revert-mode +1)
  (fset 'yes-or-no-p 'y-or-n-p)
  (load (setq custom-file "~/.emacs.d/lisp/custom.el"))
  (setq-default fill-column 80)

  (defun lg/visit-configuration ()
    "Prompt to get the configuration directory"
    (interactive)
    (let* ((default-directory user-emacs-directory))
	  (call-interactively 'find-file)))

  (defun lg/reload-configuration ()
    "(re)Load configuration file"
    (interactive)
    (mapcar
     #'(lambda (module) (load (format "~/.emacs.d/lisp/%s.el" module)))
     lg/modules))

  (defun lg/kill-this-buffer ()
    "Kill the current buffer without confirmation"
    (interactive)
    (kill-buffer (current-buffer)))

  (defun lg/poweroff-computer ()
    "Turn computer off"
    (interactive)
    (when (yes-or-no-p "Really turn off the computer ?")
      (let ((default-directory "/sudo::"))
	(shell-command "sudo poweroff"))))

  (defun efs/run-in-background (command)
    (let ((command-parts (split-string command "[ ]+")))
      (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

  (defun lg/swap-caps-control ()
    (interactive)
    (efs/run-in-background "setxkbmap gb -variant extd -option ctrl:nocaps")))

(straight-use-package '(f :type git :flavor melpa :files ("f.el" "f-shortdoc.el" "f-pkg.el") :host github :repo "rejeep/f.el"))

(provide 'core)
;;; core.el ends here
