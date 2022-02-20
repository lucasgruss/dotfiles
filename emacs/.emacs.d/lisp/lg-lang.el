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

;;;; LSP server
(use-package lsp-pyright
  :ensure-system-package (npm nodejs (pyright . "npm install -g pyright"))
  :straight t
  :after (:any eglot lsp))

;;; Formatting
(use-package apheleia
  :disabled t
  :straight (apheleia :host github :repo "raxod502/apheleia")
  :ensure-system-package ((black . "pip3 install black")
			  (clang-format . "sudo npm install -g clang-format"))
  :diminish 'apheleia-mode
  :hook (prog-mode . apheleia-mode)
  :init
  (add-to-list 'exec-path "/home/lucas/.local/bin/"))

;;; Emacs lisp
(use-package emacs
  :straight nil
  :config
  (when (featurep 'transient)
    (define-transient-command lg/transient-elisp ()
      "Emacs lisp mode"
      [["Misc"
	("e" "Eval" eval-buffer)
	("n" "Narrow" outshine-narrow-to-subtree)
	("N" "Narrow" narrow-to-defun)
	("w" "Widen" widen)]]
      [:hide (lambda () t)])

    (general-define-key
     :states 'normal
     :keymaps 'emacs-lisp-mode-map
     "<localleader>" 'lg/transient-elisp)))

;;;; eldoc
(use-package eldoc
  :straight nil
  :diminish (global-eldoc-mode eldoc-mode))

;;; Graphviz
(use-package graphviz-dot-mode :straight t :defer t)
(use-package company-graphviz-dot :defer 10)

;;; Yaml
(use-package yaml
  :defer t
  :straight t)

;;;; Yaml-mode
(use-package yaml-mode
  :straight t
  :mode ("\\.yml\\'" . yaml-mode))

;;; Matlab
(use-package matlab-mode
  :disabled t
  :straight t)

;;; lua
(use-package lua-mode
  :straight t)

(provide 'lg-lang)
