;;; lg-lang.el : configuration for programming language

;;; * LSP mode
(use-package lsp-mode
  :straight t)

;;; * Python
(use-package lsp-pyright
  :ensure-system-package (npm nodejs (pyright . "npm install -g pyright"))
  :straight t
  :hook (python-mode . lsp))

(use-package emacs
  :config
  (setq python-shell-interpreter "/home/lucas/.local/bin/ipython"))

;;; * Formatting
(use-package apheleia
  :ensure-system-package (black . "pip3 install black")
  :straight (apheleia :host github :repo "raxod502/apheleia")
  :init
  (add-to-list 'exec-path "/home/lucas/.local/bin/")
  (apheleia-global-mode +1))

;;; * Emacs lisp
(use-package eldoc
  :straight nil
  :diminish (global-eldoc-mode eldoc-mode))

;;; * Graphviz
(use-package graphviz-dot-mode :straight t)
(use-package company-graphviz-dot)
