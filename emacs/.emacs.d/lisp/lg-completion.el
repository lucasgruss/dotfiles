;;; lg-completion --- completion configuration -*- lexical-binding: t; -*-
;; Author: Lucas Gruss
;; This file is NOT part of GNU Emacs.
;;
;;; Commentary:
;; Configure completions for emacs: minibuffer, in-buffer and snippets.
;;
;;; Code:

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

(use-package prescient
  :straight t
  :config (prescient-persist-mode +1))

;;; Minibuffer-completion
;; Nice config tips : https://kristofferbalintona.me/posts/vertico-marginalia-all-the-icons-completion-and-orderless/
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
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy) ; Correct file path when changed
  :demand t
  :config
  ;; Prefix the current candidate with “» ”. From
  ;; https://github.com/minad/vertico/wiki#prefix-current-candidate-with-arrow
  (advice-add #'vertico--format-candidate :around
	      (lambda (orig cand prefix suffix index _start)
		(setq cand (funcall orig cand prefix suffix index _start))
		(concat
		 (if (= vertico--index index)
		     (propertize "» " 'face 'vertico-current)
		   "  ")
		 cand)))
  (vertico-mode +1)
  ;; extensions
  (vertico-mouse-mode +1))

(use-package orderless
  :straight t
  :custom (completion-styles '(orderless)))

(use-package marginalia
  :straight t
  :custom (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light))
  :config (marginalia-mode +1))

(use-package consult
  :straight t
  :ensure-system-package (rg . ripgrep)
  :bind ("s-\'" . consult-buffer)
  :init
  (defun lg/consult-use-package ()
    "Consult the use-package forms in the configuration."
    (interactive)
    (consult-ripgrep "~/.emacs.d/lisp" "(use-package "))
  :custom
  (consult-preview-key nil))

(use-package consult-spotify
  :straight t
  :after espotify)

(use-package embark
  :straight t
  :commands embark-act
  :bind ("s-;" . embark-act)
  :config
  ;; https://github.com/oantolin/embark/wiki/Additional-Configuration
  (defun embark-which-key-indicator ()
    "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
    (lambda (&optional keymap targets prefix)
      (if (null keymap)
	  (which-key--hide-popup-ignore-command)
	(which-key--show-keymap
	 (if (eq (plist-get (car targets) :type) 'embark-become)
	     "Become"
	   (format "Act on %s '%s'%s"
		   (plist-get (car targets) :type)
		   (embark--truncate-target (plist-get (car targets) :target))
		   (if (cdr targets) "…" "")))
	 (if prefix
	     (pcase (lookup-key keymap prefix 'accept-default)
	       ((and (pred keymapp) km) km)
	       (_ (key-binding prefix 'accept-default)))
	   keymap)
	 nil nil t (lambda (binding)
		     (not (string-suffix-p "-argument" (cdr binding))))))))

  (setq embark-indicators
	'(embark-which-key-indicator
	  embark-highlight-indicator
	  embark-isearch-highlight-indicator))

  (defun embark-hide-which-key-indicator (fn &rest args)
    "Hide the which-key indicator immediately when using the completing-read prompter."
    (which-key--hide-popup-ignore-command)
    (let ((embark-indicators
	   (remq #'embark-which-key-indicator embark-indicators)))
      (apply fn args)))

  (advice-add #'embark-completing-read-prompter
	      :around #'embark-hide-which-key-indicator))

(use-package embark-consult
  :straight t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;;; In-buffer completion (completions at point)
(use-package corfu
  :straight t
  :custom
  (corfu-cycle t)                   ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                    ;; Enable auto completion
  (corfu-auto-prefix 4)             ;; Enable auto completion
  (corfu-auto-delay 0.3)            ;; Enable auto completion
  (corfu-separator ?\s)             ;; Orderless field separator
  (corfu-quit-no-match 'separator)  ;; Never quit, even if there is no match
  (corfu-preview-current t)         ;; Disable current candidate preview
  (corfu-preselect-first nil)       ;; Disable candidate preselection
  (corfu-on-exact-match nil)        ;; Configure handling of exact matches
  (corfu-echo-documentation nil)    ;; Disable documentation in the echo area
  (corfu-scroll-margin 5)           ;; Use scroll margin
  :bind (:map corfu-map
	      ("C-j" . corfu-next)
	      ("C-k" . corfu-previous)
	      ("C-l" . corfu-complete))
  :init
  (define-key evil-insert-state-map (kbd "C-k") nil)  ;; TODO remove once evil-collection supports corfu
  (global-corfu-mode)
  ;; :config
  ;; (advice-add 'corfu--setup :after 'evil-normalize-keymaps)
  ;; (advice-add 'corfu--teardown :after 'evil-normalize-keymaps)
  ;; (evil-make-overriding-map corfu-map)
  )

(use-package corfu-doc
  :straight (:host github :type git :repo "galeo/corfu-doc")
  :bind (:map corfu-map ("C-h" . #'corfu-doc-toggle))
  :hook (corfu-mode . corfu-doc-mode))

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
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-tex)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

;; (use-package company
;;   :straight t
;;   :diminish company-mode
;;   :defer 10
;;   :config (global-company-mode +1))

;; (use-package company-box
;;   :diminish company-box-mode
;;   :straight t
;;   :after company
;;   :hook (company-mode . company-box-mode))

;; (use-package company-prescient
;;   :straight t
;;   :after company
;;   :config
;;   (company-prescient-mode +1))

;; (use-package company-ledger
;;   :straight t
;;   :disabled t
;;   :after (company ledger)
;;   :config
;;   (add-to-list 'company-backends 'company-ledger))

;;; Snippets
(use-package yasnippet
  :straight t
  :diminish yas-minor-mode
  :hook (org-mode . yas-minor-mode-on)
        (prog-mode . yas-minor-mode-on))

(use-package yasnippet-snippets ;; collection of 3rd-party snippets
  :straight t
  :after yasnippet)

(use-package consult-yasnippet
  :straight t
  :bind (:map evil-normal-state-map ("gs" . consult-yasnippet)))

(provide 'lg-completion)
;;; lg-completion.el ends here
