;;; lg-transient --- Global menu with transient.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Configuration for transient, implementing a menu system.
;;; Code:

;;; Transient
(use-package transient
  :demand t
  :config
  (my-leader-def :keymaps 'override "" 'lg/transient-root)

  (transient-define-prefix lg/transient-root ()
    "Main transient, accessed through SPC"
    [["Quick access"
      ("SPC" "M-x" execute-extended-command)
      ;("S-SPC" "M-X : buffer" execute-extended-command-for-buffer)
      ("j" "Avy" avy-goto-char-timer)
      ("k" "Org capture" org-capture)
      ("m" "Local leader" evil-send-localleader)]
     ["Dispatch"
      ("b" "Buffers/Bookmarks" lg/transient-b)
      ("c" "Consult/Code" lg/transient-c)
      ("e" "Emms" lg/transient-e)
      ("f" "Files" lg/transient-f)
      ("h" "Help" lg/transient-h)
      ("M" "Manage" lg/transient-M)]
     [""
      ("o" "Open/Org" lg/transient-o)
      ("p" "Project" lg/transient-p)
      ("q" "Quit" lg/transient-q)
      ("s" "Search/Sidebar/Snippet" lg/transient-s)
      ("t" "Toggle" lg/transient-t)
      ("w" "Window" lg/transient-w)]]
    [:hide (lambda () t)])

  (transient-define-prefix lg/transient-b ()
    "Buffers"
    [["Buffers"
      ("b" "Switch to buffer" switch-to-buffer)
      ("i" "iBuffer" ibuffer)
      ("o" "Switch to buffer in other window" switch-to-buffer-other-window)]
     ["Bookmarks"
      ("j" "Jump" bookmark-jump)
      ("l" "List bookmarks" bookmark-bmenu-list)
      ("m" "Mark" bookmark-set)
      ("s" "Set" bookmark-save)]]
    [:hide (lambda () t)])

  (transient-define-prefix lg/transient-c ()
    "Completion"
    [["Consult"
      ("r" "Consult ripgrep" consult-ripgrep)]
     ["Code"
      ("c" "Comment region" comment-region)]
     ["Checking spelling"
      ("f" "flyspell" flyspell-correct-wrapper)]
     ["Citar"
      ("o" "Open citar resource" citar-open)
      ("n" "Open citar note" citar-open-notes)]]
    [:hide (lambda () t)])

  (transient-define-prefix lg/transient-e ()
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
      ("S" "Shuffle" emms-shuffle)]
     ["Emojis"
      ("i" "Insert + search" emoji-describe)
      ("i" "Insert + search" emoji-search)
      ("I" "Insert + transient" emoji-insert)]]
    [:hide (lambda () t)])

  (transient-define-prefix lg/transient-f ()
    "Files"
    [["Files"
      ("f" "Open file" find-file)
      ("l" "Ledger file" (lambda () (interactive) (find-file "~/Documents/factures/ledger/2022/depenses.ledger")))
      ("o" "Find in other window" find-file-other-window)
      ("r" "Recent" consult-recent-file)
      ("s" "Save" save-buffer)]
     ["Configuration files"
      ("d" "dotfiles" (lambda () (interactive) (dired "~/dotfiles/")))
      ("e" ".emacs.d" lg/visit-configuration)
      ("p" "Package" lg/consult-use-package)]]
    [:hide (lambda () t)])

  (transient-define-prefix lg/transient-h ()
    "Help"
    [["Help and documentation"
      ("c" "Command (helpful)" helpful-command)
      ("f" "Functions (helpful)" helpful-callable)
      ("F" "Face" describe-face)
      ("h" "Thing at point (helpful)" helpful-at-point)
      ("i" "Info" info)
      ("k" "Key (helpful)" helpful-key)
      ("m" "Mode" describe-mode)
      ("o" "Symbol (helpful)" helpful-symbol)
      ("p" "Package" describe-package)
      ("v" "Variable (helpful)" helpful-variable)
      ("w" "Woman" woman)]
     ["Helper functions"
      ("r" "Reload configuration" lg/reload-configuration)
      ("t" "Change theme" load-theme)]
     ["Profiling"
      ("s" "Start up profiler (esup)" esup)
      ("P" "Start profiler" profiler-start)
      ("S" "Stop profiler" profiler-stop)
      ("R" "Profiler report" profiler-report)]]
    [:hide (lambda () t)])

  (transient-define-prefix lg/transient-M ()
    "Manage"
    [["System packages"
      ("i" "Install package" system-packages-install)
      ("s" "Search packages" apt-utils-show-package)
      ("u" "Update packages" system-packages-update)]
     ["Emacs packages"
      ("p" "Pull recipes (straight.el)" straight-pull-recipe-repositories)
      ("r" "Refresh (package.el)" package-refresh-contents)]]
    [:hide (lambda () t)])

  (transient-define-prefix lg/transient-o ()
    "Open/Org"
    [["Open"
      ("A" "Application" app-launcher-run-app)
      ("b" "Bluetooth" bluetooth-list-devices)
      ("c" "Calendar" calendar)
      ("C" "Calc" calc)
      ("d" "Dired" dired)
      ("D" "Disk usage" disk-usage)
      ("C-d" "Disk usage" disk-usage)
      ("e" "ERC" lg/connect-irc)
      ("f" "Elfeed" elfeed)
      ("g" "Magit" magit-status)
      ;;("N" "Enwc (Network manager)" enwc)
      ("m" "Mail (mu4e)" mu4e)
      ("M" "Open-street-map" osm-home)
      ("p" "Pass" pass)
      ("P" "Proced" proced)
      ("r" "Ripgrep" rg)
      ("s" "Smudge" smudge-my-playlists)
      ;("t" "Terminal" vterm)
      ("u" "Disk-usage" disk-usage)
      ;;("s" "Smudge" lg/transient-smudge)
      ("w" "Eww" eww)
      ("W" " Weather (wttrin)" wttrin)]
     ["Org"
      ("a" "Agenda" org-agenda)
      ("i" "Clock in" org-clock-in)
      ("j" "Org journal" org-journal-new-entry)
      ("k" "Capture" org-capture)
      ("n" "Org-noter session" org-noter)
      ("o" "Clock out" org-clock-out)
      ("R" "Org-Roam" org-roam-ui-open)]]
    [:hide (lambda () t)])

  (transient-define-prefix lg/transient-p ()
    "Project"
    [["Project"
      ("b" "Project switch to buffer" project-switch-to-buffer)
      ("c" "Compile" project-compile)
      ("d" "Dired" project-dired)
      ("f" "Find file" project-find-file)]]
    [:hide (lambda () t)])

  (transient-define-prefix lg/transient-q ()
    "Quit"
    [["Quit"
      ("e" "Exit Emacs" (lambda () (interactive) (when (y-or-n-p "Really exit Emacs ?") (kill-emacs))))
      ("q" "Turn computer off" lg/poweroff-computer)]]
    [:hide (lambda () t)])

  (transient-define-prefix lg/transient-s ()
    "Search/sidebars"
    [["Search"
      ("O" "Occur" occur)
      ("s" "Consult line" consult-line)
      ("r" "Ripgrep" rg)]
     ["Sidebars"
      ("b" "iBuffer-sidebar" ibuffer-sidebar-toggle-sidebar)
      ("d" "Dired-sidebar" dired-sidebar-toggle-sidebar)
      ("m" "Emms-sidebar" lg/find-music-directory)
      ("o" "Org-sidebar" org-sidebar-toggle)
      ("p" "Emms-playlist-sidebar" lg/emms-go-playlist)]
     ["Snippets"
      ("i" "Insert snippet" consult-yasnippet)]]
    [:hide (lambda () t)])
  
  (transient-define-prefix lg/transient-t ()
    "Toggle / Activate"
    [["Toggle"
      ("c" "Center" lg/toggle-visual-fill-center)
      ("d" "Decoration" lg/toggle-frame-decorations)
      ("f" "Fullscreen" toggle-frame-fullscreen)
      ("F" "Fullscreen all frames" lg/toggle-all-frames-fullscreen)
      ("l" "Toggle line numbers" lg/display-line-numbers-mode-enable)
      ("m" "Hide modeline" hide-mode-line-mode)
      ("M" "Hide modeline globally" global-hide-mode-line-mode)
      ("t" "Transparency" lg/toggle-transparency)
      ("s" "Scroll bars (in this frame)" toggle-scroll-bar)
      ("S" "Scroll bars (all frames)" scroll-bar-mode)
      ("v" "Visual fill column" visual-fill-column-mode)
      ("V" "Visual fill column globally" visual-fill-column-mode)
      ("z" "Zen mode" lg/zen-mode)]
     ["Modes"
      ("C-f" "Fringes" fringe-mode)
      ("h" "Hl-line" hl-line-mode)
      ("H" "Hl-line (global)" global-hl-line-mode)
      ]
     ["Activate"
      ("k" "Set caps to control" lg/swap-caps-control)]]
    [:hide (lambda () t)])

  (transient-define-prefix lg/transient-w ()
    "Windows"
    [["Window"
      ("h" "Focus left window" windmove-left)
      ("j" "Focus bottom window" windmove-down)
      ("k" "Focus top window" windmove-up)
      ("l" "Focus right window" windmove-right)]]
    [:hide (lambda () t)]))

(provide 'lg-transient)
