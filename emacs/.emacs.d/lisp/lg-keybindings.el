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
  (my-leader-def :keymaps 'override "" 'lg/transient-root)
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

;;;; Evil-collection-webkit
(use-package evil-collection-webkit
  :after webkit
  :config
  (evil-collection-xwidget-setup))

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
  (which-key-mode +1))

;;; Hercules
(use-package hercules
  :straight t)

;;; Transient
(use-package transient
  :config
  (define-transient-command lg/transient-root ()
    "Main transient, accessed through SPC"
    [["Quick access"
      ("SPC" "M-x" execute-extended-command)]
     ["Dispatch"
      ("b" "Buffers" lg/transient-b)
      ("e" "Emms" lg/transient-e)
      ("f" "Files" lg/transient-f)
      ("h" "Help" lg/transient-h)
      ("o" "Open/Org" lg/transient-o)
      ("q" "Quit" lg/transient-q)
      ("s" "Search/Sidebar" lg/transient-s)]]
    [:hide (lambda () t)])

  (define-transient-command lg/transient-b ()
    "Buffers"
    [["Buffers"
      ("b" "Switch to buffer" switch-to-buffer)
      ("i" "iBuffer" ibuffer)
      ("o" "Switch to buffer in other window" switch-to-buffer-other-window)]
     ["Bookmarks"
      ("j" "Save" bookmark-jump)
      ("s" "Save" bookmark-save)]]
    [:hide (lambda () t)])

  (define-transient-command lg/transient-c ()
    "Completion"
    [["Consult"
      ("r" ("Consult ripgrep") consult-ripgrep)]]
    [:hide (lambda () t)])

  (define-transient-command lg/transient-e ()
    "Emms"
    [["Emms"
      ("a" "Show all" emms-show-all)
      ("b" "Music library (sidebar)" lg/find-music-directory)
      ("e" "Pause" emms-pause)
      ("j" "Next" emms-next)
      ("k" "Previous" emms-previous)
      ("p" "Playlist (sidebar)" lg/emms-go-playlist)
      ("r" "Radios" emms-streams)
      ("s" "Stop" emms-stop)
      ("S" "Shuffle" emms-shuffle)]]
    [:hide (lambda () t)])

  (define-transient-command lg/transient-f ()
    "Files"
    [["Files"
      ("f" "Open file" find-file)
      ("o" "Find in other window" find-file-other-window)
      ("r" "Recent" consult-recent-file)
      ("s" "Save" save-buffer)]
     ["Configuration files"
      ("d" "dotfiles" (lambda () (interactive) (dired "~/dotfiles")))
      ("e" ".emacs.d" lg/visit-configuration)
      ("p" "Package" lg/consult-use-package)]]
    [:hide (lambda () t)])

  (define-transient-command lg/transient-h ()
    "Help"
    [["Help and documentation"
      ("c" "Command (helpful)" helpful-command)
      ("f" "Functions (helpful)" helpful-callable)
      ("h" "Thing at point (helpful)" helpful-at-point)
      ("i" "Info" info)
      ("k" "Key (helpful)" helpful-key)
      ("m" "Mode" describe-mode)
      ("o" "Symbol (helpful)" helpful-symbol)
      ("p" "Package" describe-package)
      ("v" "Variable (helpful)" helpful-variable)]
     ["Helper functions"
      ("r" "Reload configuration" lg/reload-configuration)
      ("t" "Change theme" load-theme)]
     ["Profiling"
      ("s" "Start up profiler (esup)" esup)
      ("P" "Start profiler" profiler-start)
      ("S" "Stop profiler" profiler-stop)
      ("R" "Profiler report" profiler-report)]]
    [:hide (lambda () t)])

  (define-transient-command lg/transient-o ()
    "Open/Org"
    [["Open"
      ("a" "Application" app-launcher-run-app)
      ("b" "Bluetooth" bluetooth-list-devices)
      ("c" "Calendar" calendar)
      ("C" "Calc" calc)
      ("d" "Dired" dired)
      ("f" "Elfeed" elfeed)
      ("g" "Magit" magit-status)
      ("p" "Pass" pass)
      ("P" "Proced" proced)
      ("r" "Ripgrep" rg)
      ("t" "Terminal" vterm)
      ("u" "Disk-usage" disk-usage)
      ;;("s" "Smudge" lg/transient-smudge)
      ("w" "Eww" eww)]
     ["Org"
      ("A" "Agenda" org-agenda)]]
    [:hide (lambda () t)])

  (define-transient-command lg/transient-q ()
    "Quit"
    [["Quit"
      ("e" "Exit emacs" (lambda () (interactive) (when (y-or-n-p "Really exit emacs ?") (kill-emacs))))
      ("q" "Turn computer off" lg/poweroff-computer)]]
    [:hide (lambda () t)])

  (define-transient-command lg/transient-s ()
    "Search/sidebars"
    [["Search"
      ("s" "Consult line" consult-line)
      ("r" "Ripgrep" rg)]
     ["Sidebars"
      ("b" "iBuffer-sidebar" ibuffer-sidebar-toggle-sidebar)
      ("d" "Dired-sidebar" dired-sidebar-toggle-sidebar)
      ("m" "Emms-sidebar" lg/find-music-directory)
      ("o" "Org-sidebar" org-sidebar-toggle)
      ("p" "Emms-sidebar" lg/emms-go-playlist)]]
    [:hide (lambda () t)]))

(provide 'lg-keybindings)
