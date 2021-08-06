;;; lg-exwm.el : my configuration for exwm
;; Author: Lucas Gruss

;;; Desktop-environment
(use-package desktop-environment
  :straight t
  :diminish
  :ensure-system-package (scrot brightnessctl slock)
  :config
  (desktop-environment-exwm-set-global-keybindings t)
  (general-def
    :keymaps 'desktop-environment-mode-map
    "s-l" nil)
  (setq desktop-environment-screenshot-directory "~/Images/screenshots/")
  (desktop-environment-mode +1))

;;; Ednc
(use-package ednc
  :straight t
  :disabled t
  :diminish ednc-mode
  :config
  (ednc-mode +1))


;;; EXWM
(use-package exwm
  :straight t
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

  (defun lg/exwm-init-hook ()
    (interactive)
    (efs/run-in-background "setxkbmap gb -variant extd -option ctrl:nocaps")
    (efs/run-in-background "xset r rate 300 40")
    (efs/run-in-background "killall pasystray")
    (efs/run-in-background "killall blueman-applet")
    (efs/run-in-background "killall nm-applet")
    (efs/run-in-background "killall compton")
    (efs/run-in-background "killall xsettingsd")
    (efs/run-in-background "pasystray")
    (efs/run-in-background "compton")
    (efs/run-in-background "blueman-applet")
    (efs/run-in-background "nm-applet")
    (efs/run-in-background "xsettingsd")
    (efs/run-in-background "spotifyd --config-path ~/.config/spotifyd/spotifyd.conf")
    (efs/start-panel))

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
          ([s-return] . vterm-toggle)
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
  (exwm-enable))

;;;; EXWM-randr
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

;;;; lg-exwm-utils
(use-package lg-exwm-utils
  :load-path "~/.emacs.d/lisp/site-packages")

;;;; exwm-firefox-evil
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

  (general-def :states 'insert :keymaps 'exwm-mode-map
    "<return>" #'exwm-firefox-intercept-return
    "C-h" #'exwm-firefox-core-left
    "C-j" #'exwm-firefox-core-down
    "C-k" #'exwm-firefox-core-up
    "C-l" #'exwm-firefox-core-right))

;;;; lg-exwm-polybar
(use-package lg-exwm-polybar
  :load-path "~/.emacs.d/lisp/site-packages"
  :demand t
  :hook
  (minibuffer-setup . lg/polybar-minibuffer-hide)
  (minibuffer-exit . lg/polybar-minibuffer-show))

;;;; exwm-outer-gaps
(use-package exwm-outer-gaps
  :straight (exwm-outer-gaps :host github :repo "lucasgruss/exwm-outer-gaps")
  :after (xelb exwm)
  :demand t
  :hook
  (minibuffer-setup . lg/exwm-outer-gaps-show-minibuffer-init-hook)
  (minibuffer-exit . lg/exwm-outer-gaps-hide-minibuffer-exit-hook)
  :config
  (defvar exwm-outer-gaps-polybar-timer nil
    "Timer to trigger redisplay of polybar on the minibuffer")
  (setq exwm-outer-gaps-polybar-timer
        (run-with-timer 1 nil (lambda () (efs/start-panel))))
  (setq exwm-outer-gaps-increment-step 10)
  (defvar exwm-outer-gaps-keymap nil
    "keymap to resize gaps")
  (setq exwm-outer-gaps-keymap (make-sparse-keymap))

  (defvar lg/exwm-outer-gaps--is-minibuffer-shown t
    "Whether minibuffer is hidden away or not")

  (defun lg/exwm-outer-gaps-toggle-minibuffer ()
    (interactive)
    (lg/exwm-outer-gaps-hide-show-minibuffer lg/exwm-outer-gaps--is-minibuffer-shown)
    (setq lg/exwm-outer-gaps--is-minibuffer-shown
          (not lg/exwm-outer-gaps--is-minibuffer-shown)))

  (defun lg/exwm-outer-gaps-hide-show-minibuffer (hide)
    "Hide or show the minibuffer by ajusting the bottom gap.
Argument hide is t if minibuffer should be hidden, true if shown."
    (if hide
        (exwm-outer-gaps-set 3 -20 nil)
                                        ;(setq exwm-outer-gaps-width [0 0 0 -20])
      ;; (setq exwm-outer-gaps-width [0 0 0 0]))
      (exwm-outer-gaps-set 3 0 nil))
    (exwm-outer-gaps-apply))

  (defun lg/exwm-outer-gaps-show-minibuffer ()
    "Show the minibuffer"
    (interactive)
    (lg/exwm-outer-gaps-hide-show-minibuffer nil)
    (efs/start-panel))

  (defun lg/exwm-outer-gaps-hide-minibuffer ()
    "Hide the minibuffer"
    (interactive)
    (lg/exwm-outer-gaps-hide-show-minibuffer t)
    (efs/kill-panel))

  (defun lg/exwm-outer-gaps-show-minibuffer-init-hook ()
    (when (not lg/exwm-outer-gaps--is-minibuffer-shown)
      (lg/exwm-outer-gaps-hide-show-minibuffer nil)))

  (defun lg/exwm-outer-gaps-hide-minibuffer-exit-hook ()
    (when (not lg/exwm-outer-gaps--is-minibuffer-shown)
      (lg/exwm-outer-gaps-hide-show-minibuffer t)))

  ;; (map! (:map exwm-outer-gaps-keymap
  ;;        :desc "Decrease left" "h" (lambda () (interactive) (exwm-outer-gaps-decrement 0))
  ;;        :desc "Increase left" "H" (lambda () (interactive) (exwm-outer-gaps-increment 0))
  ;;        :desc "Decrease right" "l" (lambda () (interactive) (exwm-outer-gaps-decrement 1))
  ;;        :desc "Increase right" "L" (lambda () (interactive) (exwm-outer-gaps-increment 1))
  ;;        :desc "Decrease top" "k" (lambda () (interactive) (exwm-outer-gaps-decrement 2))
  ;;        :desc "Increase top" "K" (lambda () (interactive) (exwm-outer-gaps-increment 2))
  ;;        :desc "Decrease bottom" "j" (lambda () (interactive) (exwm-outer-gaps-decrement 3))
  ;;        :desc "Increase bottom" "J" (lambda () (interactive) (exwm-outer-gaps-increment 3))
  ;;        :desc "Shift frame left" "y" (lambda () (interactive)
  ;;                                       (exwm-outer-gaps-increment 1)
  ;;                                       (exwm-outer-gaps-decrement 0))
  ;;        :desc "Shift frame right" "o" (lambda () (interactive)
  ;;                                        (exwm-outer-gaps-increment 0)
  ;;                                        (exwm-outer-gaps-decrement 1))
  ;;        :desc "Shift frame down" "u" (lambda () (interactive)
  ;;                                       (exwm-outer-gaps-increment 2)
  ;;                                       (exwm-outer-gaps-decrement 3))
  ;;        :desc "Shift frame up" "i" (lambda () (interactive)
  ;;                                     (exwm-outer-gaps-increment 3)
  ;;                                     (exwm-outer-gaps-decrement 2))))

  ;; (defun lg/exwm-outer-gaps-hercules ()
  ;;   (interactive))

  ;; (hercules-def
  ;;  :toggle-funs #'lg/exwm-outer-gaps-hercules
  ;;  :keymap 'exwm-outer-gaps-keymap
  ;;  :transient t)

  (defun lg/exwm-outer-gaps-setenv-and-polybar ()
    "Set environment variables that are used by polybar to overlay
  the minibuffer, and restart polybar after a timer."
    (setenv "GAP_RIGHT" "50%")
    (setenv "GAP_BOTTOM" (number-to-string
                          (if exwm-outer-gaps-mode
                              (aref exwm-outer-gaps-width 3)
                            0)))
    (setenv "PANEL_WIDTH" (concat "50%:-"
                                  (number-to-string
                                   (if exwm-outer-gaps-mode
                                       (aref exwm-outer-gaps-width 0)
                                     0))))
    (cancel-timer exwm-outer-gaps-polybar-timer)
    (setq exwm-outer-gaps-polybar-timer
          (run-with-timer 1 nil (lambda () (efs/start-panel)))))

  (advice-add #'exwm-outer-gaps-apply :before #'lg/exwm-outer-gaps-setenv-and-polybar)
  (exwm-outer-gaps-mode +1))

(provide 'lg-exwm)
