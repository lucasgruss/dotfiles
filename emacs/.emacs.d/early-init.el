;;; early-init.el : Expensive stuff early on

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq package-enable-at-startup nil)
(set-face-attribute 'default nil :family "Iosevka" :weight 'normal :height 110)
(set-face-attribute 'fixed-pitch nil :family "Iosevka" :weight 'normal :height 110)
(set-face-attribute 'variable-pitch nil :family "Roboto" :weight 'semi-light :height 110 :width 'normal)
(server-start)
