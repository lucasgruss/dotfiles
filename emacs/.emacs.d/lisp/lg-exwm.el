;;; lg-exwm.el : my configuration for exwm

(use-package exwm
  :straight t
  ;;:init (add-to-list native-comp-def)
  :config
  (use-package dash
    :straight t)
  (defun lg/exwm-async-run (name)
    "Run a process asynchronously"
    (interactive)
    (start-process name nil name))

  (defun lg/run-or-raise-or-dismiss (program program-buffer-name)
    "If no instance of the program is running, launch the program.
If an instance already exists, and its corresponding buffer is
displayed on the screen, move to the buffer. If the buffer is not
visible, switch to the buffer in the current window. Finally, if
the current buffer is already that of the program, bury the
buffer (=minimizing in other WM/DE)"
    ;; check current buffer
    (if (string= (buffer-name) program-buffer-name)
        (bury-buffer)
      ;; either switch to or launch program
      (progn
        (if (get-buffer program-buffer-name)
            (progn
              (if (get-buffer-window program-buffer-name)
                  (select-window (display-buffer program-buffer-name) nil)
                (exwm-workspace-switch-to-buffer program-buffer-name)))
          ;; start program
          (progn
            (lg/exwm-async-run program)
            (message (format "Launching %s" program)))))))

  (defun lg/run-or-raise-or-dismiss-firefox ()
    (interactive)
    (lg/run-or-raise-or-dismiss "firefox" "Firefox-esr"))

  (defun lg/run-or-raise-or-dismiss-firefox ()
    (interactive)
    (let ((buf-list (->> (buffer-list)
                      (--reject (not (string= "Firefox-esr" (buffer-local-value 'exwm-class-name it))))))
          buf)
      (if buf-list
          (progn
            (setq buf (car buf-list))
            (if (eq (current-buffer) buf)
                (windower-switch-to-last-buffer)
              (if (get-buffer-window buf)
                  (select-window (display-buffer buf) nil)
                (exwm-workspace-switch-to-buffer buf))))
        ;; start program
        (lg/exwm-async-run "firefox")
        (message (format "Launching %s" "firefox")))))

  (defun lg/run-or-raise-or-dismiss-spotify ()
    (interactive)
    (lg/run-or-raise-or-dismiss "spotify" "Spotify"))

  (defun lg/run-or-raise-or-dismiss-thunderbird ()
    (interactive)
    (lg/run-or-raise-or-dismiss "thunderbird" "thunderbird"))

  (defun lg/toggle-line-char-modes ()
    "If on a EXWM buffer, toggle 'line' or 'char'"
    (interactive)
    (if exwm-window-type
        (if (string= exwm--input-mode "line-mode")
            (call-interactively #'exwm-input-release-keyboard) ; switch to char mode
          (call-interactively #'exwm-input-grab-keyboard)))) ; switch to line mode

  (defun lg/lock-screen ()
    "Lock screen with slock"
    (interactive)
    (start-process "" nil "/usr/local/bin/slock"))

  (defun efs/run-in-background (command)
    (let ((command-parts (split-string command "[ ]+")))
      (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

  (defvar efs/polybar-process nil
    "Holds the process of the running Polybar instance, if any")

  (defun efs/kill-panel ()
    (interactive)
    (when efs/polybar-process
      (ignore-errors
        (kill-process efs/polybar-process)))
    (setq efs/polybar-process nil))

  (defun efs/start-panel ()
    (interactive)
    (efs/kill-panel)
    (setq efs/polybar-process (start-process-shell-command "polybar" nil "polybar -r panel")))

  (defun lg/exwm-init-hook ()
    (interactive)
    (shell-command "setxkbmap gb -variant extd -option ctrl:nocaps")
    (shell-command "xset r rate 300 40")
    ;; (shell-command "killall pasystray")
    ;; (shell-command "killall blueman-applet")
    ;; (shell-command "killall nm-applet")
    ;;(shell-command "killall compton")
    ;; (shell-command "killall kdeconnect-indicator")
    ;; (shell-command "killall xsettingsd")
                                        ;(shell-command "feh --bg-fill ~/Images/Wallpaper/landscapes_sand_desert_dunes.jpg")
    ;; (shell-command "feh --bg-fill ~/Images/Wallpaper/solid_blue.jpeg")
    ;; ;; (shell-command "feh --bg-fill ~/Images/Wallpaper/mountain.png")
    ;; (efs/run-in-background "pasystray")
    (efs/run-in-background "compton")
    (efs/run-in-background "xfce4-power-manager")
    (efs/run-in-background "xfce4-panel")
    (efs/run-in-background "blueman-applet")
    (efs/run-in-background "nm-applet")
    ;; (efs/run-in-background "kdeconnect-indicator")
    (efs/run-in-background "xsettingsd")
    ;; (efs/start-panel))
    )

  (add-hook 'exwm-init-hook #'lg/exwm-init-hook)

  (defun lg/exwm-update-title-hook ()
    "Hook to be ran when window title is updated"
    (if (not (string= exwm-class-name "Firefox-esr"))
        (exwm-workspace-rename-buffer exwm-class-name)
      (exwm-workspace-rename-buffer exwm-title)))

  (add-hook 'exwm-update-title-hook #'lg/exwm-update-title-hook)

  ;; disable tab bar for floating frames
  (add-hook 'exwm-floating-setup-hook
            (lambda ()
              (toggle-tab-bar-mode-from-frame -1)))

  (setq exwm-input-global-keys
        `(([S-s-backspace] . exwm-workspace-delete)
          ([?\s-f] . exwm-layout-toggle-fullscreen)
          ([?\s-F] . exwm-floating-toggle-floating)
          ([?\s-R] . exwm-reset)
          ([?\s-w] . exwm-utils-workspace-switch-cyclically)
          ([?\s-W] . exwm-utils-workspace-move-cyclically)
          ([?\s-\'] . consult-buffer)
          ([?\s-\@] . ibuffer)
          ([?\s-b] . bury-buffer)
          ([s-f2]  . lg/lock-screen)
          ([?\s-d] . app-launcher-run-app)
          ;; ([?\s-d] . (lambda (command)
          ;;                (interactive (list (read-shell-command "$ ")))
          ;;                (start-process-shell-command command nil command)))
          ([?\s-i] . lg/run-or-raise-or-dismiss-firefox)
          ([?\s-t] . lg/run-or-raise-or-dismiss-thunderbird)
          ([?\s-s] . lg/run-or-raise-or-dismiss-spotify)
          ([?\s-u] . lg/toggle-line-char-modes)
          ([s-return] . vterm)
          ([s-escape] . lg/kill-this-buffer)
    

      ([?\s-/]  . centaur-tabs-mode)
          ([?\s-m]  . centaur-tabs-backward)
          ([?\s-,]  . centaur-tabs-forward)
          ([?\s-?]  . tab-bar-mode)
          ([?\s-M]  . lg/tab-previous-and-hide-maybe)
          ([?\s-<]  . lg/tab-next-and-hide-maybe)
          ([?\s-O]  . exwm-outer-gaps-mode)
          ([?\s-y]  . exwm-outer-gaps-increment)
          ([?\s-p]  . exwm-outer-gaps-decrement)
          ;; Everything window
          ([?\s-q] . evil-window-delete)
          ([?\s-v] . split-window-horizontally)
          ([?\s-z] . split-window-vertically)
          ([s-tab]  . windower-switch-to-last-buffer)
          ([?\s-r]  . windower-switch-to-last-buffer)
          ([?\s-\\] . windower-toggle-split)
          ([?\s-o]  . windower-toggle-single)
          ([142606440] . windower-move-border-left) ; M-s-h
          ([142606442] . windower-move-border-below); M-s-j
          ([142606443] . windower-move-border-above); M-s-k
          ([142606444] . windower-move-border-right); M-s-l
          ([?\s-h] . windmove-left)  ([?\s-H] . windower-swap-left)
          ([?\s-j] . windmove-down)  ([?\s-J] . windower-swap-below)
          ([?\s-k] . windmove-up)    ([?\s-K] . windower-swap-above)
          ([?\s-l] . windmove-right) ([?\s-L] . windower-swap-right)))
  (setq exwm-workspace-show-all-buffers t)
  (setq exwm-layout-show-all-buffers t)
  (setq exwm-workspace-number 2)
  (setq exwm-workspace-minibuffer-position nil)
  (setq exwm-workspace-display-echo-area-timeout 1)

  (setq exwm-manage-configurations
         '(((or (equal "hl2-linux" exwm-class-name)
                (equal "hl2-linux" exwm-title))
            floating nil
            fullscreen nil
            tiling-mode-line nil
            ;;char-mode t
            managed t)
           ((equal exwm-class-name "openspades")
            floating nil
            managed t)
           ((equal exwm-class-name "Firefox-esr")
            floating-mode-line nil)))

  (push (aref (kbd "<escape>") 0) exwm-input-prefix-keys)
  (push (aref (kbd "<return>") 0) exwm-input-prefix-keys)
  (push (aref (kbd "s-<SPC>") 0) exwm-input-prefix-keys)

  ;; relaunch the panel so that it auto-hide correctly
  ;;(shell-command "xfce4-panel")
  (exwm-enable))

(use-package exwm-randr
  :after exwm
  :config
  (setq exwm-randr-workspace-monitor-plist '(0 "eDP-1" 1 "DP-2"))
  (setq exwm-randr-screen-change-hook nil)
  (defun lg/setup-screens ()
    (start-process-shell-command
     "xrandr" nil "sh ~/.screenlayout/arandr.layout.sh"))
  (add-hook 'exwm-randr-screen-change-hook #'lg/setup-screens)
  (exwm-randr-enable))

(use-package lg-exwm-utils
  :load-path "~/.emacs.d/lisp/")

(use-package exwm-firefox-evil
  :straight t
  :hook ((exwm-manage-finish . exwm-firefox-evil-activate-if-firefox)
         ;; (exwm-manage-finish . lg/exwm-firefox-force-fullscreen)
         (exwm-firefox-evil-mode . lg/exwm-firefox-hook))
  :init
  (setq exwm-firefox-evil-firefox-class-name '("Firefox" "Firefox-esr" "Nightly" "Iceweasel" "Icecat"))
  :config
  (defun exwm-input--on-ButtonPress-line-mode (buffer button-event)
    "Handle button events in line mode.
BUFFER is the `exwm-mode' buffer the event was generated
on. BUTTON-EVENT is the X event converted into an Emacs event.

The return value is used as event_mode to release the original
button event."
    (with-current-buffer buffer
      (let ((read-event (exwm-input--mimic-read-event button-event)))
        (exwm--log "%s" read-event)
        (if (and read-event
                 (exwm-input--event-passthrough-p read-event))
            ;; The event should be forwarded to emacs
            (progn
              (exwm-input--cache-event read-event)
              (exwm-input--unread-event button-event)
              xcb:Allow:ReplayPointer)
          ;; xcb:Allow:SyncPointer)
          ;; The event should be replayed
          xcb:Allow:ReplayPointer))))

  (defun lg/exwm-firefox-force-fullscreen ()
    "Send F11 to firefox to always be in full screen.

Whenever you switch to another window and then come back to
firefox, it leaves fullscreen mode."
    (interactive)
    (exwm-input--fake-key 'f11))

  (defun lg/exwm-firefox-toggle-tree-tab ()
    "Toggle the tree tab extension"
    (interactive)
    (exwm-input--fake-key 'f1))

  (defun lg/exwm-firefox-hook ()
    "Hook to be run after entering exwm-firefox-evil-mode"
    (interactive)
    (exwm-firefox-intercept-next-ret)
    (exwm-firefox-evil-insert))

  (defun lg/exwm-firefox-hint ()
    "Highlights hints on the page."
    (interactive)
    (exwm-input--fake-key 'C-m)
    (exwm-firefox-evil-insert))

  (defvar exwm-firefox-next-ret-normal)

  ;; go back to normal mode after pressing return
  (defun exwm-firefox-intercept-next-ret ()
    (interactive)
    (setq-local exwm-firefox-next-ret-normal t))

  (defun exwm-firefox-intercept-return ()
    (interactive)
    (exwm-input--fake-key (aref (kbd "<return>") 0))
    (when (and (boundp 'exwm-firefox-next-ret-normal)
               exwm-firefox-next-ret-normal)
      (exwm-firefox-evil-normal)
      (setq-local exwm-firefox-next-ret-normal nil)))

  (advice-add #'exwm-firefox-core-tab-new :after #'exwm-firefox-intercept-next-ret)
  (advice-add #'lg/exwm-firefox-hint :after #'exwm-firefox-intercept-next-ret)
  (advice-add #'exwm-firefox-core-focus-search-bar :after #'exwm-firefox-intercept-next-ret)
  (advice-add #'exwm-firefox-core-quick-find :after #'exwm-firefox-intercept-next-ret)

  (evil-initial-state 'exwm-firefox-evil-mode 'insert)

  (general-def :states 'normal :keymaps 'exwm-firefox-evil-mode-map
    "m" nil
    "t" #'exwm-firefox-core-window-new
    "f" #'lg/exwm-firefox-hint
    "F" #'lg/exwm-firefox-force-fullscreen
    "T" #'lg/exwm-firefox-toggle-tree-tab
    "q" #'exwm-input-send-next-key
    "<return>" #'exwm-firefox-intercept-return)

  ;; (general-def :keymaps 'exwm-mode-map
  ;;   "<return>" #'(lambda ()
  ;;                  (interactive)
  ;;                  (unless exwm-firefox-evil-mode
  ;;                    (exwm-input--fake-key (aref (kbd "<return>") 0)))))

  (general-def :states 'insert :keymaps 'exwm-mode-map
    "<return>" #'exwm-firefox-intercept-return
    "C-h" #'exwm-firefox-core-left
    "C-j" #'exwm-firefox-core-down
    "C-k" #'exwm-firefox-core-up
    "C-l" #'exwm-firefox-core-right))


;; (use-package exwm-firefox
;;   :straight t
;;   :after exwm-firefox-evil
;;   :config
;;   (defun exwm-firefox-open-in-mpv ()
;;     (interactive)
;;     (exwm-firefox-core-focus-search-bar)
;;     (exwm-firefox-core-copy)
;;     (emms-play-url (current-kill 0 nil)))
;;   ;; I have different keybinding in firefox for tabdetach-attach : M-S-t
;;   (defun lg/exwm-firefox-attach ()
;;     "Attach the current tab into its parent window.

;;    This requires the tabdetach extension to work."
;;     (interactive)
;;     (exwm-input--fake-key ?\M-\S-T))

;;   (define-key! 'normal exwm-firefox-evil-mode-map
;;     "A" #'lg/exwm-firefox-attach
;;     "D" #'exwm-firefox-split-detach
;;     "M" #'exwm-firefox-merge)
;;   ;; I don't like renaming the name of the firefox window
;;                                         ;(remove-hook 'exwm-update-title-hook 'exwm-firefox--update-title))
;;   )

(defun lg/polybar-minibuffer-hide ()
  (call-process "polybar-msg" nil 0 nil "cmd" "hide"))
(defun lg/polybar-minibuffer-show ()
  (call-process "polybar-msg" nil 0 nil "cmd" "show"))

(use-package app-launcher
  :straight   (app-launcher :type git :host github
			    :repo "SebastienWae/app-launcher")
  :bind ("s-d" . 'app-launcher-run-app))

(provide 'lg-exwm)
