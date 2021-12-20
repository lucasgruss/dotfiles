;; -*- lexical-binding: t; -*-
;;; lg-lang.el : configuration for programming language

;;; LSP mode
(use-package lsp-mode
  :straight t
  :after python-mode)

;;; Python
(use-package lsp-pyright
  :ensure-system-package (npm nodejs (pyright . "npm install -g pyright"))
  :straight t
  :hook (python-mode . lsp))

(use-package emacs
  :config
  (setq python-shell-interpreter "/home/lucas/.local/bin/ipython"))

;;; Formatting
(use-package apheleia
  :straight (apheleia :host github :repo "raxod502/apheleia")
  :ensure-system-package ((black . "pip3 install black")
			  (clang-format . "sudo npm install -g clang-format"))
  :diminish apheleia-mode
  :init
  (add-to-list 'exec-path "/home/lucas/.local/bin/")
  (apheleia-global-mode +1))

;;; Emacs lisp
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

(provide 'lg-lang)
