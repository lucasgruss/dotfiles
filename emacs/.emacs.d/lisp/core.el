;;; core.el : core packages needed for my configuration

(server-start)

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

;; USE-PACKAGE
(straight-use-package 'use-package)
(use-package bind-key :straight t) ;; needed for the :bind keyword
(use-package use-package-ensure-system-package :straight t) ;; needed for the :ensure-system-package keyword
(use-package diminish :straight t)  ;; needed for the :diminish keyword

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

;;;; Esup : emacs statup time profiler
(use-package esup
  :straight t)

;;; Package.el
;; built-in package manager
(use-package package
  :straight nil
  :defer 5
  :commands (describe-package)
  :config (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/")))

(use-package emacs
  :straight nil
  :config
  (fset 'yes-or-no-p 'y-or-n-p)
  (setq idle-update-delay 0.3)
  (setq comp-async-report-warnings-errors nil)
  (setq make-backup-files nil)
  (setq ring-bell-function 'ignore)
  (setq x-select-enable-primary t)
  (setq select-enable-clipboard t)
  (setq inhibit-startup-screen t)
  (setq y-or-n-p-use-read-key t)
  (setq x-select-enable-clipboard-manager t)
  (setq recentf-max-saved-items 100)
  (setq custom-file "~/.emacs.d/lisp/custom.el")
  (load-file custom-file)

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

  (recentf-mode +1)
  (show-paren-mode +1)
  :bind ("s-<escape>" . 'lg/kill-this-buffer)
  :general
  (general-def :states 'normal :keymaps 'Info-mode-map
    "RET" 'Info-follow-nearest-node))

(provide 'core)
