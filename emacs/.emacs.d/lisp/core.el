;;; core.el core packages needed for my configuration

(setq gc-cons-threshold (* 50 1000 1000)) ;; try and speed up startup time

;; magically bootstrap straight
(defvar bootstrap-version)
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


;; USE-PACKAGE
(straight-use-package 'use-package)
(eval-when-compile
  (require 'use-package))
(use-package bind-key)
(use-package system-packages :straight t)
(use-package use-package-ensure-system-package :straight t)
(use-package diminish :straight t)  ;; remove clutter in the modeline

;; built-in package manager
(use-package package
  :config
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/")))

(use-package emacs
  :init
  (fset 'yes-or-no-p 'y-or-n-p)
  ;;(setq initial-buffer-choice "*Messages*")
  (setq idle-update-delay 0.3)
  (setq comp-async-report-warnings-errors nil)
  (setq make-backup-files nil)
  (setq ring-bell-function 'ignore)
  (setq x-select-enable-primary t)
  (setq select-enable-clipboard t)
  (setq inhibit-startup-screen t)
  (setq y-or-n-p-use-read-key t)
  (setq x-select-enable-clipboard-manager t)

  (defun lg/visit-configuration ()
    "Prompt to get the configuration directory"
    (interactive)
    (let* ((default-directory user-emacs-directory))
	  (call-interactively 'find-file)))

  (defun emacs-startup-time-info ()
    "Profile emacs startup"
    (message "*** Emacs loaded in %s with %d garbage collections."
             (format "%.2f seconds"
                     (float-time
                      (time-subtract after-init-time before-init-time)))
             gcs-done))

  (defun lg/kill-this-buffer ()
    "Kill the current buffer without confirmation"
    (interactive)
    (kill-buffer (current-buffer)))

 :bind ("s-<escape>" . 'lg/kill-this-buffer)
 :hook (emacs-startup . emacs-startup-time-info))

(provide 'core)
