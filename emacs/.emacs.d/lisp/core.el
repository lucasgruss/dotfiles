;;; core.el : core packages needed for my configuration

(setq native-comp-async-report-warnings-errors nil)

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
(use-package bind-key :straight t) ;; needed for the :bind keyword
(use-package use-package-ensure-system-package :straight t) ;; needed for the :ensure-system-package keyword
(use-package diminish :straight t)  ;; needed for the :diminish keyword

(use-package general ;; needed for the :general keyword
  :straight t
  :config
  (general-create-definer my-leader-def :states '(normal visual motion) :prefix "SPC")
  (general-create-definer my-local-leader-def :states '(normal visual motion) :prefix "SPC m")
  (my-leader-def
   :keymaps 'override
   "" '(nil :which-key "Leader prefix")
   "i" '(nil :which-key "Insert\n")
   "s" '(nil :which-key "Search\n")
   "f" '(nil :which-key "Files\n")
   "ff" '(find-file :which-key "Find file")
   "fp" '(lg/visit-configuration :which-key "Find private configuration")
   "fs" '(save-buffer :which-key "Save file")
   "b" '(nil :which-key "Buffer\n")
   "bb" '(switch-to-buffer :which-key "Buffer")
   "bo" '(switch-to-buffer-other-window :which-key "Buffer")
   "h" (general-key "C-h");'(lg/general-def-wrap-C-h :which-key "Help\n")
   "ht" 'load-theme
   "hr" 'lg/reload-configuration
   "hb" nil
   "t" '(nil :which-key "Toggle\n")
   "o" '(nil :which-key "Open\n")
   "SPC" '(execute-extended-command :which-key "M-x")))

;; PERFORMANCES (as soon as possible)
;; (setq gc-cons-threshold (* 4 100 1024 1024)) ;; try and speed up startup time
(use-package gcmh
  :straight t
  :diminish gcmh-mode
  :config
  (gcmh-mode +1))

;; built-in package manager
(use-package package
  :straight nil
  :defer 5
  :commands (describe-package)
  :config (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/")))

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
  :bind ("s-<escape>" . 'lg/kill-this-buffer))

(provide 'core)
