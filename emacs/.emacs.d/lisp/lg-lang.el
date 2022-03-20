;;; lg-lang.el --- configuration for programming language -*- lexical-binding: t; -*-

;;; Tooling
;;;; LSP mode
(use-package lsp-mode
  :disabled t
  :straight t
  :unless (featurep 'eglot)
  :defer t
  :hook (python-mode . lsp)
  :custom (read-process-output-max (* 1024 1024)) ;; 1mb
  :init (setq lsp-keymap-prefix "s-x"))

;;;; eglot
(use-package eglot
  :straight t
  :defer t
  :unless (featurep 'lsp-mode)
  :hook (python-mode . eglot)
  :custom
  (read-process-output-max (* 1024 1024)) ;; 1mb
  (eglot-autoreconnect 3))

;;;; DAP : debugging
(use-package dap-mode
  :straight t
  :defer t
  :custom
  (dap-auto-configure-features '(sessions locals controls tooltip)))

;;;; DAP-python
(use-package dap-python
  :ensure-system-package (ptvsd . "pip install \"ptvsd>=4.2\"")
  :straight nil
  :after dap-mode)

;;; Python
(use-package python
  :defer t
  :custom
  (python-shell-interpreter "/usr/bin/python3"))

;;;; pylint
(use-package pylint
  :straight t
  :ensure-system-package pylint
  :general
  (:keymaps 'python-mode-map
	    :states 'normal
	    "<localleader>l" #'pylint))

;;;; LSP server
(use-package lsp-pyright
  :ensure-system-package (npm nodejs (pyright . "npm install -g pyright"))
  :straight t
  :after (:any eglot lsp))

;;; Formatting
;;;; apheleia
(use-package apheleia
  :disabled t
  :straight (apheleia :host github :repo "raxod502/apheleia")
  :ensure-system-package ((black . "pip3 install black")
			  (clang-format . "sudo npm install -g clang-format"))
  :diminish 'apheleia-mode
  :hook (prog-mode . apheleia-mode)
  :init
  (add-to-list 'exec-path "/home/lucas/.local/bin/"))

;;;; c-style
(use-package cc-vars ; indentation and overall style
  :defer t
  :custom
  (c-default-style 
    '((java-mode . "java")
      (awk-mode . "awk")
      (c++-mode . "stroustrup")
      (other . "gnu"))))

;;; Emacs lisp
(use-package emacs ;; emacs-lisp
  :straight nil
  :general
  (:keymaps 'emacs-lisp-mode-map
	    :states 'normal
	    "<localleader>e" #'eval-buffer
	    "<localleader>l" #'elint
	    "<localleader>n" #'outshine-narrow-to-subtree
	    "<localleader>N" #'narrow-to-defun
	    "<localleader>w" #'widen))
;; :config
;; (when (featurep 'transient)
;;   (define-transient-command lg/transient-elisp ()
;;     "Emacs lisp mode"
;;     [["Misc"
;; 	("e" "Eval" eval-buffer)
;; 	("n" "Narrow" outshine-narrow-to-subtree)
;; 	("N" "Narrow" narrow-to-defun)
;; 	("w" "Widen" widen)]]
;;     [:hide (lambda () t)])

;;   (general-define-key
;;    :states 'normal
;;    :keymaps 'emacs-lisp-mode-map
;;    "<localleader>" 'lg/transient-elisp)))

;;;; eldoc
(use-package eldoc
  :straight nil
  :diminish (global-eldoc-mode eldoc-mode)
  :config
  (global-eldoc-mode +1))

;;;; elisp-indent-docstrings-mode
(use-package elisp-indent-docstrings-mode
  :config
  (elisp-indent-docstrings-mode +1))

;;; Graphviz
(use-package graphviz-dot-mode :straight t :defer t)
(use-package company-graphviz-dot :defer t :if (featurep 'company))

;;; Yaml
(use-package yaml
  :defer t
  :straight t)

;;;; Yaml-mode
(use-package yaml-mode
  :straight t
  :mode ("\\.yml\\'" . yaml-mode))

;;; Matlab
(use-package matlab
  :straight matlab-mode
  :defer t
  :ensure-system-package (matlab . matlab-support)
  :custom
  (matlab-shell-command-switches '("-nodesktop" "-nosplash"))
  :config
  (evil-define-key 'visual matlab-mode-map "gr" #'lg/matlab-mode-eval-region)

  (defun lg/matlab-mode-eval-region (start end)
    (interactive "r")
    (let ((region-str (buffer-substring-no-properties start end)))
      (with-current-buffer (concat "*" matlab-shell-buffer-name "*")
	(insert region-str)
	(comint-send-input))))

  (defun lg/matlab-eval-buffer ()
    (interactive)
      (with-current-buffer (concat "*" matlab-shell-buffer-name "*")
	(insert buffer-file-name)
	(comint-send-input))))

;;; lua
(use-package lua-mode
  :straight t)

;;; markdown
  (use-package markdown-mode
    :straight t
    :ensure-system-package pandoc
    :custom
    (markdown-command "pandoc")
   (markdown-live-preview-window-function ))

(provide 'lg-lang)
