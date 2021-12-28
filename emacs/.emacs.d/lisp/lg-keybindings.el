;; -*- lexical-binding: t; -*-
;;; lg-keybindings: basic configuration needed for the keybindings
;; Author: Lucas Gruss

;;; Evil-mode
(use-package evil
  :straight t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-Y-yank-to-eol t)
  (setq evil-undo-system 'undo-redo)
  :bind (:map evil-visual-state-map ("gr" . eval-last-sexp))
  :config (evil-mode +1))

;;; Evil-collection
(use-package evil-collection
  :diminish evil-collection-unimpaired-mode
  :straight t
  :after evil
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
  :config
  (when (featurep 'transient)
    (general-define-key
     :states 'normal 
     :keymaps 'evil-ledger-mode-map
      "<localleader>" 'lg/transient-ledger)))

;;; evil-org
(use-package evil-org
  :straight (:type git :host github :repo "Somelauw/evil-org-mode")
  :hook (org-mode . evil-org-mode)
  :commands (org-agenda)
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;;; Evil-anzu
(use-package evil-anzu
  :straight t
  :diminish anzu-mode
  :commands (evil-search-forward evil-search-backward)
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

(use-package evil-goggles
  :straight t
  :diminish evil-goggles-mode
  :after evil
  :hook (prog-mode . evil-goggles-mode)
  :custom
  (evil-goggles-enable-surround t)
  (evil-goggles-duration 0.3))

;; evil-surround
(use-package evil-surround
  :straight t
  :after evil
  :config (global-evil-surround-mode +1))

;; evil-numbers
(use-package evil-numbers
  :straight t
  :after evil
  :config
  (evil-define-key '(normal visual) 'global (kbd "g +") 'evil-numbers/inc-at-pt)
  (evil-define-key '(normal visual) 'global (kbd "g -") 'evil-numbers/dec-at-pt)
  (evil-define-key '(normal visual) 'global (kbd "g C-+") 'evil-numbers/inc-at-pt-incremental)
  (evil-define-key '(normal visual) 'global (kbd "g C--") 'evil-numbers/dec-at-pt-incremental))

;;; Which-key
(use-package which-key
  :diminish which-key-mode
  :straight t
  :custom (which-key-idle-delay 0.7)
  :config (which-key-mode +1))

;;; Hercules
(use-package hercules
  :straight t)

(provide 'lg-keybindings)
