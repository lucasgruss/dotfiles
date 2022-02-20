;;; lg-completion --- completion configuration -*- lexical-binding: t; -*-
;; Author: Lucas Gruss

(use-package emacs
  :custom
  (tab-always-indent 'complete)
  :init
  (defun emacs-execute-extended-command ()
    "Create and select a frame called emacs-run-launcher which
consists only of a minibuffer and has specific dimensions. Run
counsel-linux-app on that frame, which is an emacs command that
prompts you to select an app and open it in a dmenu like
behaviour. Delete the frame after that command has exited"
    (interactive)
    (with-selected-frame (make-frame '((name . "emacs-run-launcher")
				       (minibuffer . only)
				       (undecorated . t)
				       (width . 0.4)
				       (height . 11)
				       (top . 0.5)
				       (left . 0.5)))
      (unwind-protect
	  (execute-extended-command nil)
	(delete-frame)))))

;;;Selectrum
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
  (selectrum-mode -1))

;;; Prescient
(use-package prescient
  :straight t
  :config
  (prescient-persist-mode +1))

;;; selectrum-prescient
(use-package selectrum-prescient
  :straight t
  :after (selectrum prescient)
  :custom
  (selectrum-prescient-enable-filtering nil)
  :config
  (when (featurep 'selectrum)
    (selectrum-prescient-mode +1)))

;;; vertico
(use-package vertico
  :straight '(vertico :files (:defaults "extensions/*")
		      :includes (vertico-buffer
				 vertico-directory
				 vertico-flat
				 vertico-indexed
				 vertico-mouse
				 vertico-quick
				 vertico-repeat
				 vertico-reverse))
  :bind (:map vertico-map
	      ("C-h" . vertico-directory-delete-word)
	      ("C-j" . vertico-next)
	      ("C-k" . vertico-previous)
	      ("C-l" . vertico-insert))
  :demand t
  :config
  (vertico-mode +1)
  ;; extensions
  (vertico-mouse-mode +1))

;;; Orderless
(use-package orderless
  :straight t
  :config
  (when (featurep 'selectrum)
    (setq orderless-skip-highlighting (lambda () selectrum-is-active)))
  (setq completion-styles '(orderless)))

;;; Marginalia
(use-package marginalia
  :straight t
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light))
  :config
  (marginalia-mode +1)
  (when (featurep 'all-the-icons-completion)
    (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup)))

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
(use-package consult-selectrum :after (selectrum consult))

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

  (when (featurep 'selectrum)
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

    (add-hook 'embark-collect-mode-hook #'shrink-selectrum)))

;;; embark-consult
(use-package embark-consult
  :straight t
  :after embark)

;;; Company
;; (use-package company
;;   :straight t
;;   :diminish company-mode
;;   :defer 10
;;   :config (global-company-mode +1))

;; ;;; Company-box
;; (use-package company-box
;;   :diminish company-box-mode
;;   :straight t
;;   :after company
;;   :hook (company-mode . company-box-mode))

;; ;;; Company-prescient
;; (use-package company-prescient
;;   :straight t
;;   :after company
;;   :config
;;   (company-prescient-mode +1))

;; ;;; Company-ledger
;; (use-package company-ledger
;;   :straight t
;;   :disabled t
;;   :after company
;;   :config
;;   (add-to-list 'company-backends 'company-ledger))

;;; corfu
(use-package corfu
  :straight t
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-prefix 4)                 ;; Enable auto completion
  (corfu-auto-delay 0.3)                 ;; Enable auto completion
  (corfu-separator ?\s)          ;; Orderless field separator
  (corfu-quit-no-match 'separator)      ;; Never quit, even if there is no match
  (corfu-preview-current t)    ;; Disable current candidate preview
  (corfu-preselect-first nil)    ;; Disable candidate preselection
  (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  (corfu-echo-documentation t) ;; Disable documentation in the echo area
  (corfu-scroll-margin 5)        ;; Use scroll margin
  :bind (:map corfu-map
	      ("C-j" . corfu-next)
	      ("C-k" . corfu-previous)
	      ("C-l" . corfu-complete))
  :init
  (define-key evil-insert-state-map (kbd "C-k") nil)  ;; TODO remove once evil-collection supports corfu
  (corfu-global-mode +1)
  ;; :config
  ;; (advice-add 'corfu--setup :after 'evil-normalize-keymaps)
  ;; (advice-add 'corfu--teardown :after 'evil-normalize-keymaps)
  ;; (evil-make-overriding-map corfu-map)
  )

;;;; corfu-doc
(use-package corfu-doc
  :disabled t
  :straight (:host github :type git :repo "galeo/corfu-doc")
  :hook
  (corfu-mode . corfu-doc-mode))

;;;; cape
(use-package cape
  :straight t
  ;; Bind dedicated completion commands
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-symbol)
         ("C-c p a" . cape-abbrev)
         ("C-c p i" . cape-ispell)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p \\" . cape-tex)
         ("C-c p _" . cape-tex)
         ("C-c p ^" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345))
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-ispell)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line))
  )

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
  :after yasnippet
  :bind (:map evil-normal-state-map
	      ("gs" . consult-yasnippet)))

(provide 'lg-completion)
