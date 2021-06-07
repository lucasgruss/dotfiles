;; lg-keybindings.el: basic configuration needed for the keybindings
;; evil mode, general and more

(use-package general
  :straight t
  :config
  ;;(general-evil-setup)
  ;; (general-create-definer my-leader-def :keymaps '(normal visual) :prefix "SPC")
  ;; (general-create-definer my-local-leader-def :keymaps '(normal visual) :prefix "SPC m")
  (general-create-definer my-leader-def :states '(normal visual motion) :prefix "SPC")
  (general-create-definer my-local-leader-def :states '(normal visual motion) :prefix "SPC m")
  (my-leader-def
   :prefix "SPC" :states 'normal :keymaps 'override
   "" '(nil :which-key "my lieutenant general prefix")
   "f" '(:ignore t :which-key "Files")
   "ff" '(find-file :which-key "Find file")
   "fp" '(lg/visit-configuration :which-key "Find file")
   "fs" '(save-buffer :which-key "Save file")
   "b" '(:ignore t :which-key "Buffer")
   "bb" '(switch-to-buffer :which-key "Buffer")
   "bo" '(switch-to-buffer-other-window :which-key "Buffer")
   "h" (general-key "C-h")
   "ht" 'load-theme))

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
  :config (evil-esc-mode +1))

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
