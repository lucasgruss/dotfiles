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

;;; use-package
(straight-use-package 'use-package)

(use-package use-package
  :straight t
  :custom
  (use-package-compute-statistics t)
  (use-package-verbose nil))

;;;; bind-key
;; needed for the :bind-key keyword
(use-package bind-key
  :straight t)

;;;; use-package-ensure-system-package
;; needed for the :ensure-system-package keyword
(use-package use-package-ensure-system-package
  :straight t
  :defer t)

;;;; diminish
;; needed for the :diminish keyword
(use-package diminish
  :straight t
  :config
  (diminish 'auto-fill-function))

;;; straight-x
(use-package straight-x
  :straight nil)

;;; PERFORMANCES (as soon as possible)
;; (setq gc-cons-threshold (* 4 100 1024 1024)) ;; try and speed up startup time

;;;; GCMH : Garbage collector magic hack
(use-package gcmh
  :straight t
  :diminish gcmh-mode
  :config
  (gcmh-mode +1))

;;; no-littering
(use-package no-littering
  :straight t
  :after recentf
  :config
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))

;;;; Esup : emacs startup time profiler
(use-package esup
  :straight t
  :commands esup)

;;; Package.el
;; built-in package manager
(use-package package
  :straight nil
  :defer t
  :commands (describe-package)
  :config
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  (add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/")))

;;; Super save mode
(use-package super-save
  :straight t
  :diminish super-save-mode
  :config (super-save-mode +1))

;;; recentf-mode
(use-package recentf
  :custom
  (recentf-max-saved-items 1000)
  :defer 5
  :config
  (recentf-mode +1))

;;; so-long
(use-package so-long
  :config
  (global-so-long-mode +1))

;;; Emacs
(use-package emacs ;; core
  :straight nil
  :diminish (auto-revert-mode eldoc-mode)
  :bind
  ("s-<escape>" . 'lg/kill-this-buffer)
  ("s-b" . 'bury-buffer)
  :custom
  (idle-update-delay 0.3)
  (delete-by-moving-to-trash t) 
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
  (context-menu-mode +1)
  (show-paren-mode +1)
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
