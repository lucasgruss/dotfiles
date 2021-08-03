;;; lg-keybindings: basic configuration needed for the keybindings
;; Author: Lucas Gruss

;;; Evil-mode
(use-package evil
  :straight t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-redo)
  :config
  ;; declaration the leader map through general.el
  (my-leader-def
    :keymaps 'override
    "" '(nil :which-key "Leader prefix")
    "c" (general-key "C-c")
    "x" (general-key "C-x")
    "i" '(nil :which-key "Insert\n")
    "s" '(nil :which-key "Search\n")
    "f" '(nil :which-key "Files\n")
    "ff" '(find-file :which-key "Find file\n")
    "fo" '(find-file-other-window :which-key "Find file in other window\n")
    "fp" '(lg/visit-configuration :which-key "Find private configuration\n")
    "fs" '(save-buffer :which-key "Save file\n")

    ;; consult 
    "fr" '(consult-recent-file :which-key "Find-recent-file\n")
    "fc" '(lg-consult-use-package :which-key "Find package config \n")
    "bb" 'consult-buffer
    "bo" 'consult-buffer-other-window
    "ss" 'consult-line

    ;; buffers
    "b" '(nil :which-key "Buffer\n")
    "bb" '(switch-to-buffer :which-key "Switch to buffer\n")
    "bo" '(switch-to-buffer-other-window :which-key "Buffer other window\n")
    "bi" '(ibuffer :which-key "iBuffer\n")

    ;; help
    "h" (general-key "C-h")
    "ht" 'load-theme
    "hr" 'lg/reload-configuration
    "hb" nil
    "SPC" '(execute-extended-command :which-key "M-x\n")

    ;; sidebars
    "t" '(nil :which-key "Toggle\n")
    "tb" '(ibuffer-sidebar-toggle-sidebar :which-key "Toggle iBuffer-sidebar\n")
    "td" '(dired-sidebar-toggle-sidebar :which-key "Toggle dired-sidebar\n")
    "to" '(org-sidebar-toggle :which-key "Toggle org-sidebar\n")

    ;; Open
    "o" '(nil :which-key "Open\n")
    "ob" '(bluetooth-list-devices :which-key "Bluetooth\n")
    "of" '(elfeed :which-key "Elfeed\n")
    "oF" '(elfeed-update :which-key "Update elfeed\n")
    "om" '(mu4e :which-key "Open mail - mu4e\n")
    "op" '(pass :which-key "Pass\n")
    "ow" '(eww-browse-with-history :which-key "Eww with history")
    "og" '(magit-status :which-key "Magit\n")

    ;; pass
    "ip" '(password-store-copy :which-key "Copy password")

    ;; emms
    "e" '(nil :which-key "emms\n")
    "ej" '(emms-next :which-key "Next song\n")
    "ek" '(emms-previous :which-key "Previous song\n")
    "es" '(emms-stop :which-key "Stop\n")
    "ee" '(emms-pause :which-key "Pause/play\n")
    "eS" '(emms-shuffle :which-key "Shuffle\n")
    "ea" '(emms-show-all :which-key "Show all\n")
    "eb" '(lg/find-music-directory :which-key "Music library (sidebar)\n")
    "em" '(lg/emms-go-playlist :which-key "Emms playlist\n")
    "er" '(emms-streams :which-key "Emms streams\n")

    ;; helpful
    "hh" 'helpful-at-point
    "hc" 'helpful-command
    "hf" 'helpful-callable
    "hk" 'helpful-key
    "ho" 'helpful-symbol
    "hv" 'helpful-variable
    )

  ;; activate evil-mode
  (evil-mode +1))

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

;;; Evil-ledger
(use-package evil-ledger
  :straight t
  :hook (ledger-mode . evil-ledger-mode)
  :after ledger-mode)

;;; Evil-anzu
(use-package evil-anzu
  :straight t
  :diminish anzu-mode
  :config (global-anzu-mode +1))

;;; Evil-escape
(use-package evil-escape
  :straight t
  :diminish evil-escape-mode
  :custom
  (evil-escape-key-sequence "jk")
  (evil-escape-excluded-major-modes '(magit-status-mode))
  :config
  (evil-escape-mode +1))

;;; Which-key
(use-package which-key
  :diminish which-key-mode
  :straight t
  :custom
  (which-key-idle-delay 0.7)
  :config
  (which-key-mode +1)
  ;; fix how SPC h appears in which-key
  (which-key-add-key-based-replacements
    "SPC c" "C-c\n")
  (which-key-add-key-based-replacements
    "SPC h" "Help\n")
  (which-key-add-key-based-replacements
    "SPC x" "C-x\n"))

;;; Hercules
(use-package hercules
  :straight t)

;;; Matcha
(use-package matcha
  :disabled t
  :straight (matcha :type git :host github :repo "jojojames/matcha")
  :config (matcha-setup))

(provide 'lg-keybindings)
