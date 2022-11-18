;;; lg-web --- Some tools to interact with internet -*- lexical-binding: t; -*-
;; Author: Lucas Gruss

;;; EWW
(use-package eww
  :commands (eww eww-browse-with-history)
  ;:hook (eww-after-render . prot-eww--rename-buffer)
  :custom
  (eww-auto-rename-buffer 'title)
  (eww-download-directory "~/Téléchargements/eww/")
  (eww-desktop-data-save '(:url :title))
  :config
  (advice-add 'eww-back-url :after #'prot-eww--rename-buffer)
  (advice-add 'eww-forward-url :after #'prot-eww--rename-buffer)
  (use-package lg-eww
    :load-path "lisp/site-packages"))

;;; browse-url
(use-package browse-url
  :straight nil
  :defer t
  :custom
  (browse-url-handlers
   '(("\\`mailto:" . browse-url--mailto)
    ("\\www\.youtube\.com" . browse-url-umpv)
    ("https://youtu\.be/" . browse-url-umpv)
    ("\\`man:" . browse-url--man)
    ("\\`.mp4" . browse-url-umpv)
    (browse-url--non-html-file-url-p . browse-url-emacs)))

  :init
  (defun browse-url-umpv (url &optional single)
    (start-process "mpv" nil (if single "mpv" "umpv")
		   (shell-quote-wildcard-pattern url)))

  (defun browse-url-at-point-umpv (&optional single)
    "Open link in mpv"
    (interactive "P")
    (let ((browse-url-browser-function
	   (if single
	       (lambda (url &optional _new-window) (browse-url-umpv url t))
	     #'browse-url-umpv)))
      (browse-url-at-point))))

;;; Engine-mode
(use-package engine-mode
  :straight t
  :defer t
  :custom
  (engine/browser-function 'eww-browse-url)
  :config
  (defengine ctan "http://www.ctan.org/search/?x=1&PORTAL=on&phrase=%s")
  (defengine duckduckgo "https://duckduckgo.com/?q=%s")
  (defengine github "https://github.com/search?ref=simplesearch&q=%s")
  (defengine google "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s")
  (defengine google-images
	     "http://www.google.com/images?hl=en&source=hp&biw=1440&bih=795&gbv=2&aq=f&aqi=&aql=&oq=&q=%s")
  (defengine google-maps "http://maps.google.com/maps?q=%s")
  (defengine project-gutenberg "http://www.gutenberg.org/ebooks/search/?query=%s")
  (defengine stack-overflow "https://stackoverflow.com/search?q=%s")
  (defengine wikipedia
	     "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s")
  (defengine wiktionary
	     "https://www.wikipedia.org/search-redirect.php?family=wiktionary&language=en&go=Go&search=%s")
  (defengine youtube "http://www.youtube.com/results?aq=f&oq=&search_query=%s"
	     :browser 'browse-url-firefox)
  (engine-mode +1))

;;; wikinforg
(use-package wikinforg
  :straight t
  :defer t)

;;; wttrin
(use-package wttrin
  :straight t
  :commands wttrin
  :custom
  (wttrin-default-cities '("Paris" "Nantes" "Russange" ":help"))
  :config
  ;; https://github.com/bcbcarl/emacs-wttrin/issues/16
  (defun wttrin-fetch-raw-string (query)
    "Get the weather information based on your QUERY."
    (let ((url-user-agent "curl"))
      (add-to-list 'url-request-extra-headers wttrin-default-accept-language)
      (with-current-buffer
	  (url-retrieve-synchronously
	   (concat "http://fr.wttr.in/" query "?A")
	   (lambda (status) (switch-to-buffer (current-buffer))))
	(decode-coding-string (buffer-string) 'utf-8)))))

(provide 'lg-web)
