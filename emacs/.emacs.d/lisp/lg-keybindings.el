;;; lg-keybindings: basic configuration needed for the keybindings  -*- lexical-binding: t; -*-
;; Author: Lucas Gruss

;;; general
(use-package general
  :straight t
  :config
  (general-def :states 'normal :keymaps 'Info-mode-map
    "RET" 'Info-follow-nearest-node)
  (general-def :states '(normal visual) :keymaps 'eww-mode-map
    "i" 'evil-insert)
  (general-create-definer my-leader-def :states '(normal visual motion) :prefix "SPC")
  (general-create-definer my-local-leader-def :states '(normal visual motion) :prefix "SPC m"))

;;; Evil-mode
(use-package evil
  :straight t
  :demand t
  :init
  (setq evil-want-integration t)  ;; should be t for evil-collection
  (setq evil-want-keybinding nil) ;; should be nil for evil-collection
  (setq evil-want-Y-yank-to-eol t)
  (setq evil-undo-system 'undo-redo)
  :bind (:map evil-visual-state-map ("gr" . eval-last-sexp))
  :config (evil-mode +1))

;;; Evil-collection
(use-package evil-collection
  :diminish evil-collection-unimpaired-mode
  :straight t
  :after evil
  :custom (evil-collection-calendar-want-org-bindings t)
  :config
  (evil-collection-init))

;;;; Evil bindings for bluetooth.el
(use-package evil-collection-bluetooth
  :straight nil ;; This is a site package until I submit a PR to evil-collection
  :after bluetooth
  :config
  (evil-collection-bluetooth-setup))

;;;; Evil-collection-webkit
(use-package evil-collection-webkit
  :after webkit
  :config
  (evil-collection-xwidget-setup))

;;; Evil-ledger
(use-package evil-ledger
  :straight t
  :hook (ledger-mode . evil-ledger-mode)
  :after ledger-mode
  :init
  (when (featurep 'transient)
    (define-transient-command lg/transient-ledger ()
      "Buffers"
      [["Ledger"
	("a" "New transaction" lg/insert-transaction)
	("o" "Ledger occur / narrow" ledger-occur)
	("r" "Report" ledger-report)
	("s" "Sort" ledger-sort-buffer)]]
      [:hide (lambda () t)])
    (general-define-key
     :states '(normal motion)
     :keymaps '(evil-ledger-mode-map ledger-report-mode-map)
     "<localleader>" 'lg/transient-ledger
     "I" 'lg/insert-transaction)))

;;; evil-org
(use-package evil-org
  :straight (:type git :host github :repo "Somelauw/evil-org-mode")
  :after org
  :diminish 'evil-org-mode
  :hook (org-mode . evil-org-mode)
  :commands (org-agenda)
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;;; Evil-anzu
(use-package evil-anzu
  :straight t
  :diminish anzu-mode
  :config (global-anzu-mode +1))

;;; Evil-escape
;; see for improvement: https://blog.d46.us/advanced-emacs-startup/
(use-package evil-escape
  :straight t
  :diminish evil-escape-mode
  :commands (evil-escape-pre-command-hook)
  :hook (pre-command . evil-escape-pre-command-hook)
  :custom
  (evil-escape-key-sequence "jk")
  (evil-escape-excluded-major-modes '(magit-status-mode))
  :config (evil-escape-mode +1))

;;; evil-goggles
(use-package evil-goggles
  :straight t
  :diminish evil-goggles-mode
  :after evil
  :hook (prog-mode . evil-goggles-mode)
  :custom
  (evil-goggles-enable-surround t)
  (evil-goggles-duration 0.3))

;;; evil-surround
(use-package evil-surround
  :straight t
  :after evil
  :config (global-evil-surround-mode +1))

;;; evil-numbers
(use-package evil-numbers
  :straight t
  :after evil
  :config
  (evil-define-key '(normal visual) 'global (kbd "g +") 'evil-numbers/inc-at-pt)
  (evil-define-key '(normal visual) 'global (kbd "g -") 'evil-numbers/dec-at-pt)
  (evil-define-key '(normal visual) 'global (kbd "g C-+") 'evil-numbers/inc-at-pt-incremental)
  (evil-define-key '(normal visual) 'global (kbd "g C--") 'evil-numbers/dec-at-pt-incremental))

;; ;;; evil-mu4e
;; (use-package evil-mu4e
;;   :straight t
;;   :after mu4e)

;;; Which-key
(use-package which-key
  :diminish which-key-mode
  :straight t
  :custom
  (which-key-min-display-lines 15 "Prefer vertical display of candidates.")
  (which-key-idle-delay 0.3)
  (which-key-separator " " )
  :config
  (which-key-setup-minibuffer)
  (which-key-mode +1))

;;; Hercules
(use-package hercules
  :straight t)

(provide 'lg-keybindings)
