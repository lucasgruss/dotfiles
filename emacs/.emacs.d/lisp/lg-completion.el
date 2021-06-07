;;; lg-completion.el : completion configuration

(use-package selectrum
  :straight t
  :init
  (setq projectile-completion-system 'default)
  :config
  (setq selectrum-num-candidates-displayed 10)
  (setq selectrum-fix-minibuffer-height nil)
  (setq selectrum-display-action nil)
  (setq selectrum-refine-candidates-function #'orderless-filter)
  (setq selectrum-highlight-candidates-function #'orderless-highlight-matches)
  (selectrum-mode +1)
  ;(setf (alist-get "<escape>" selectrum-minibuffer-bindings) #'abort-recursive-edit)
  (general-def
    :keymaps 'selectrum-minibuffer-map
    "C-j" 'selectrum-next-candidate
    "s-'" 'selectrum-next-candidate
    "<mouse-5>" 'selectrum-next-candidate
    "C-k" 'selectrum-previous-candidate
    "s-@" 'selectrum-previous-candidate
    "<mouse-4>" 'selectrum-previous-candidate
    "C-l" 'selectrum-insert-current-candidate))
    ;; "<escape>" 'exit-minibuffer))


(use-package prescient
  :straight t
  :after selectrum
  :config
  (prescient-persist-mode +1))


(use-package orderless
  :straight t
  :config
  (setq orderless-skip-highlighting (lambda () selectrum-is-active))
  (setq completion-styles '(orderless)))


;; (use-package selectrum-prescient
;;   :after (selectrum prescient)
;;   :config
;;   (setq selectrum-prescient-enable-filtering nil)
;;   (selectrum-prescient-mode +1))


(use-package marginalia
  :straight t
  :init
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light))
  :config
  (marginalia-mode))


(use-package consult
  :straight t
  :general
  (general-def
   "s-\'" 'consult-buffer)
  (my-leader-def :keymaps 'override
   :prefix "SPC" :states 'normal
   "fr" 'consult-recent-file
   "bb" 'consult-buffer
   "bo" 'consult-buffer-other-window
   "ss" 'consult-line)
   :config
   (setq consult-preview-key nil))

(use-package consult-selectrum :after consult)

(use-package consult-spotify
  :straight t
  :disabled t
  :load-path "~/.config/doom/espotify/"
  :init
  (load! "~/.config/doom/private.el"))


;; (use-package embark
;;   :config
;;   (map! "s-;" #'embark-act)
;;   ;; For Selectrum users:
;;   (setq embark-action-indicator
;;         (lambda (map)
;;           (which-key--show-keymap "Embark" map nil nil 'no-paging)
;;           #'which-key--hide-popup-ignore-command)
;;         embark-become-indicator embark-action-indicator)

;;   (defun current-candidate+category ()
;;     (when selectrum-is-active
;;       (cons (selectrum--get-meta 'category)
;;             (selectrum-get-current-candidate))))

;;   (add-hook 'embark-target-finders #'current-candidate+category)

;;   (defun current-candidates+category ()
;;     (when selectrum-is-active
;;       (cons (selectrum--get-meta 'category)
;;             (selectrum-get-current-candidates
;;              ;; Pass relative file names for dired.
;;              minibuffer-completing-file-name))))

;;   (add-hook 'embark-candidate-collectors #'current-candidates+category)

;;   ;; No unnecessary computation delay after injection.
;;   (defun shrink-selectrum ()
;;     (when (eq embark-collect--kind :live)
;;       (with-selected-window (active-minibuffer-window)
;;         (setq-local selectrum-num-candidates-displayed 1)
;;         (setq-local selectrum-display-style
;;                     '(horizontal :before-candidates "[" :after-candidates "]"
;;                                  :more-candidates "" :candidates-separator "")))))

;;   (add-hook 'embark-collect-mode-hook #'shrink-selectrum) (add-hook 'embark-setup-hook 'selectrum-set-selected-candidate))

(use-package company
  :diminish company-mode
  :straight t
  :config
  (global-company-mode +1))

(provide 'lg-completion)
