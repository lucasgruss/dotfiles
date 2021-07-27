;;; lg-keybindings: basic configuration needed for the keybindings
;; Author: Lucas Gruss

;;; Evil-mode
(use-package evil
  :straight t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-redo)
  :config (evil-mode +1))

;;; Evil-collection
(use-package evil-collection
  :diminish evil-collection-unimpaired-mode
  :straight t
  :after evil
  :config
  (evil-collection-init)
  :general
  (general-def :states 'normal :keymaps 'Info-mode-map
    "RET" 'Info-follow-nearest-node))

;;; Evil-anzu
(use-package evil-anzu
  :straight t
  :diminish anzu-mode
  :config (global-anzu-mode +1))

;;; Evil-escape
(use-package evil-escape
  :straight t
  :diminish evil-escape-mode
  :init (setq evil-escape-key-sequence "jk")
  :config
  (setq evil-escape-excluded-major-modes '(magit-status-mode))
  (evil-escape-mode +1))

;;; Which-key
(use-package which-key
  :diminish which-key-mode
  :straight t
  :config
  (setq which-key-idle-delay 0.7)
  (which-key-mode +1)
  ;; fix how SPC h appears in which-key
  (which-key-add-key-based-replacements
    "SPC h" "Help\n"))

;;; Matcha
(use-package matcha
  :disabled t
  :straight (matcha :type git :host github :repo "jojojames/matcha")
  :config (matcha-setup))

(provide 'lg-keybindings)
