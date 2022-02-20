;;; core.el --- core packages needed for my configuration -*- lexical-binding: t; -*-

(setq native-comp-async-report-warnings-errors nil)
(setq native-comp-async-jobs-number 4)

;;; set *some* security settings early
;; cf https://glyph.twistedmatrix.com/2015/11/editor-malware.html
(setq tls-checktrust t)
(setq gnutls-verify-error t)

;; magically bootstrap straight
(defvar bootstrap-version)
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
(setq straight-disable-native-compilation nil)
(setq straight-disable-byte-compilation nil)
(setq straight-disable-compile nil)

;;; use-package
(straight-use-package 'use-package)

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
  :straight t)

;;; straight-x
(use-package straight-x
  :straight nil)

;;; general
(use-package general ;; needed for the :general keyword
  :straight t
  :config
  (general-create-definer my-leader-def :states '(normal visual motion) :prefix "SPC")
  (general-create-definer my-local-leader-def :states '(normal visual motion) :prefix "SPC m"))

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
  :straight t)

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
  :custom (recentf-max-saved-items 100)
  :config
  (when (featurep 'no-littering)
    (add-to-list 'recentf-exclude no-littering-var-directory)
    (add-to-list 'recentf-exclude no-littering-etc-directory))
  (recentf-mode +1))

;;; Emacs
(use-package emacs
  :straight nil
  :diminish (auto-revert-mode eldoc-mode)
  :bind
  ("s-<escape>" . 'lg/kill-this-buffer)
  ("s-b" . 'bury-buffer)
  :general
  (general-def :states 'normal :keymaps 'Info-mode-map
    "RET" 'Info-follow-nearest-node)
  (general-def :states '(normal visual) :keymaps 'eww-mode-map
    "i" 'evil-insert)
  :custom
  (idle-update-delay 0.3)
  (comp-async-report-warnings-errors nil)
  (make-backup-files nil)
  (ring-bell-function 'ignore)
  (x-select-enable-primary t)
  (select-enable-clipboard t)
  (inhibit-startup-screen t)
  (y-or-n-p-use-read-key t)
  (x-select-enable-clipboard-manager t)
  :config
  ;;(server-start)
  (show-paren-mode +1)
  (auto-revert-mode +1)
  (global-eldoc-mode +1)
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

(provide 'core)
