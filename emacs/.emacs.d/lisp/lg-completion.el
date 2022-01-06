;;; lg-completion --- completion configuration -*- lexical-binding: t; -*-
;; Author: Lucas Gruss

;;; Selectrum
(use-package selectrum
  :straight t
  :defer nil
  :init
  (setq projectile-completion-system 'default)
  :general
  (general-def
    :keymaps 'selectrum-minibuffer-map
    "C-j" 'selectrum-next-candidate
    "s-'" 'selectrum-next-candidate
    "<mouse-5>" 'selectrum-next-candidate
    "C-k" 'selectrum-previous-candidate
    "s-@" 'selectrum-previous-candidate
    "<mouse-4>" 'selectrum-previous-candidate
    "C-l" 'selectrum-insert-current-candidate
    "<escape>" 'abort-minibuffers)
  :config
  (setq selectrum-num-candidates-displayed 10)
  (setq selectrum-fix-minibuffer-height nil)
  (setq selectrum-display-action nil)
  (setq selectrum-refine-candidates-function #'orderless-filter)
  (setq selectrum-highlight-candidates-function #'orderless-highlight-matches)
  (selectrum-mode +1))

;;; Prescient
(use-package prescient
  :straight t
  :after selectrum
  :config
  (prescient-persist-mode +1))

;;; Orderless
(use-package orderless
  :straight t
  :config
  (setq orderless-skip-highlighting (lambda () selectrum-is-active))
  (setq completion-styles '(orderless)))

;;; selectrum-prescient
(use-package selectrum-prescient
  ;;:disabled t
  :straight t
  :after (selectrum prescient)
  :config
  (setq selectrum-prescient-enable-filtering nil)
  (selectrum-prescient-mode +1))

;;; Marginalia
(use-package marginalia
  :straight t
  :init
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light))
  :config
  (marginalia-mode))

;;; Consult
(use-package consult
  :straight t
  :bind ("s-\'" . consult-buffer)
  :init
  (defun lg/consult-use-package ()
    "Consult the use-package forms in the configuration."
    (interactive)
    (consult-ripgrep "~/.emacs.d/lisp" "(use-package "))
  :custom
  (consult-preview-key nil))

;;;; espotify-consult
(use-package consult-spotify
  :straight t
  :after espotify)

;;; Consult-selectrum
(use-package consult-selectrum :after consult)

;;; Embark
(use-package embark
  :straight t
  :commands embark-act
  :bind ("s-;" . embark-act)
  :config
  ;; https://github.com/oantolin/embark/wiki/Additional-Configuration
  (setq embark-action-indicator
	(lambda (map &optional _target)
	  (which-key--show-keymap "Embark" map nil nil 'no-paging)
	  #'which-key--hide-popup-ignore-command)
	embark-become-indicator embark-action-indicator)

  (defun refresh-selectrum ()
    (setq selectrum--previous-input-string nil))

  (add-hook 'embark-pre-action-hook #'refresh-selectrum) 

  (defun shrink-selectrum ()
    (when (eq embark-collect--kind :live)
      (with-selected-window (active-minibuffer-window)
	(setq-local selectrum-num-candidates-displayed 1)
	(setq-local selectrum-display-style
		    '(horizontal :before-candidates "[" :after-candidates "]"
				 :more-candidates "" :candidates-separator "")))))

  (add-hook 'embark-collect-mode-hook #'shrink-selectrum))

;;; embark-consult
(use-package embark-consult
  :straight t
  :after embark)

;;; Company
(use-package company
  :straight t
  :diminish company-mode
  :defer 10
  :config (global-company-mode +1))

;;; Company-box
(use-package company-box
  :diminish company-box-mode
  :straight t
  :after company
  :hook (company-mode . company-box-mode))

;;; Company-prescient
(use-package company-prescient
  :straight t
  :after company
  :config
  (company-prescient-mode +1))

;;; Company-ledger
(use-package company-ledger
  :straight t
  :disabled t
  :after company
  :config
  (add-to-list 'company-backends 'company-ledger))

(provide 'lg-completion)
