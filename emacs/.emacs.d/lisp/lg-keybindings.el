;; lg-keybindings.el: basic configuration needed for the keybindings
;; evil mode, general and more

(use-package evil
  :straight t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-redo)
  :config (evil-mode +1))

(use-package evil-collection
  :diminish evil-collection-unimpaired-mode
  :straight t
  :after evil
  :config (evil-collection-init))

(use-package evil-anzu
  :straight t
  :diminish anzu-mode
  :config (global-anzu-mode +1))

(use-package evil-escape
  :straight t
  :diminish evil-escape-mode
  :init (setq evil-escape-key-sequence "jk")
  :config (evil-escape-mode +1))

(use-package which-key
  :diminish which-key-mode
  :straight t
  :config
  (setq which-key-idle-delay 0.7)
  (which-key-mode +1))

(use-package matcha
  :disabled t
  :straight (matcha :type git :host github :repo "jojojames/matcha")
  :config (matcha-setup))

(provide 'lg-keybindings)
