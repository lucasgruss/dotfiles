;;; lg-org --- configuration for org -*- lexical-binding: t; -*-
;; Author: Lucas Gruss
;; This file is NOT part of GNU Emacs.
;;
;;; Commentary:
;; This is my configuration for org mode, including configuration of the builtin
;; features as well as 3rd-party packages and features to extend and improve
;; org.
;; 
;;; Code:
;;; Org
(use-package org
  :straight t
  :hook ((org-mode . auto-fill-mode)
	 ;; (before-save . org-footnote-normalize)
	 (before-save . org-table-recalculate-buffer-tables))
  :diminish org-indent-mode
  :defer t
  :magic ("\\.org" . org-mode)
  :init
  ;;https://www.emacswiki.org/emacs/WordCount
  (defun word-count-analysis (start end)
    "Count how many times each word is used in the region.
    Punctuation is ignored."
    (interactive "r")
    (let (words)
      (save-excursion
	(goto-char start)
	(while (re-search-forward "\\w+" end t)
	  (let* ((word (intern (match-string 0)))
		 (cell (assq word words)))
	    (if cell
		(setcdr cell (1+ (cdr cell)))
	      (setq words (cons (cons word 1) words))))))
      (when (interactive-p)
	(message "%S" words))
      words))

  (when (featurep 'transient)
    (define-transient-command lg/transient-org ()
      "Org mode"
      [["Misc"
	("a" "Agenda" org-agenda)
	("A" "Archive" org-archive-subtree)
	("c" "Cite" org-cite-insert)
	("e" "Export" org-export-dispatch)
	("E" "Effort" org-set-effort)
	("f" "Insert footnote" org-footnote-new)
	("h" "HTML live preview" org-preview-html-mode)
	("l" "Link" org-insert-link)
	("n" "Narrow subtree" org-toggle-narrow-to-subtree)
	("P" "Set property" org-set-property)
	("t" "Tangle file (babel)" org-babel-tangle)
	("T" "Set tag" org-set-tags-command)
	("w" "Refile" org-refile)
	("W" "Refile" org-refile-copy)]
       ["Clocking"
	("i" "Clock in" org-clock-in)
	("o" "Clock out" org-clock-out)
	("p" "Pomodoro" org-pomodoro)]
       ["Roam"
	("b" "Toggle the org-roam buffer" org-roam-buffer-toggle)
	("u" "Org-roam-ui" org-roam-ui-open)]
       ["Todo"
	("m" "Change todo state" org-todo)
	("k" "Increase priority" org-priority-up)
	("j" "Increase priority" org-priority-down)
	("s" "Schedule task" org-schedule)
	("d" "Set a deadline on task" org-deadline)]]
      [:hide (lambda () t)])

    (general-define-key
     :states 'normal 
     :keymaps 'org-mode-map
     "<localleader>" 'lg/transient-org))
  (defun org-clocking-buffer () nil) ;; without it, impossible to exit emacs with C-x C-c
  :custom
  (org-directory "~/org/")
  (org-default-notes-file "~/org/todo.org")
  (org-fontify-quote-and-verse-blocks t "Setting this variable to t makes it consistent with src blocks.")
  (org-fontify-whole-heading-line nil)
  (org-startup-with-latex-preview nil "Avoid eager evaluation of the latex code.")
  (org-hide-leading-stars t "We can't count too many stars anyway.")
  (org-startup-indented t "Activate indent-mode.")
  (org-archive-location "archive/%s_archive::")
  (org-enforce-todo-dependencies t)
  (org-archive-subtree-add-inherited-tags t "Keep tags of from parent heading.")
  (org-hide-emphasis-markers t)
  (org-src-block-faces
   `(("emacs-lisp" modus-themes-nuanced-magenta)
     ("elisp" modus-themes-nuanced-magenta)
     ("clojure" modus-themes-nuanced-magenta)
     ("clojurescript" modus-themes-nuanced-magenta)
     ("c" modus-themes-nuanced-blue)
     ("c++" modus-themes-nuanced-blue)
     ("sh" modus-themes-nuanced-green)
     ("shell" modus-themes-nuanced-green)
     ("html" modus-themes-nuanced-yellow)
     ("xml" modus-themes-nuanced-yellow)
     ("css" modus-themes-nuanced-red)
     ("scss" modus-themes-nuanced-red)
     ("matlab" modus-themes-nuanced-red)
     ("octave" modus-themes-nuanced-red)
     ("python" modus-themes-nuanced-green)
     ("ipython" modus-themes-nuanced-magenta)
     ("r" modus-themes-nuanced-cyan)
     ("yaml" modus-themes-nuanced-cyan)
     ("conf" modus-themes-nuanced-cyan)
     ("docker" modus-themes-nuanced-cyan)))
  (org-export-dispatch-use-expert-ui nil)
  (org-export-in-background nil)
  (org-return-follows-link t)
  :config 
  (push 'org-habit org-modules)
  (plist-put org-format-latex-options :scale 1.5)
  (defun lg/org-footnotes-normalize ()
    (when (equal major-mode 'org-mode)
      (org-footnote-normalize))))

(use-package org-id
  :after org
  :custom (org-id-method 'uuid)
	  (org-id-ts-format "%Y%m%dT%H%M%S"))

(use-package org-agenda
  :init
  ;; Mostly inspired from Protesilaos Stavrou's configuration  
  ;; https://protesilaos.com/codelog/2021-12-09-emacs-org-block-agenda/
  (defvar lg/org-custom-daily-agenda
    `((tags-todo "*"
		 ((org-agenda-skip-function '(org-agenda-skip-if nil '(timestamp)))
		  (org-agenda-skip-function
		   `(org-agenda-skip-entry-if
		     'notregexp ,(format "\\[#%s\\]" (char-to-string org-priority-highest))))
		  (org-agenda-block-separator nil)
		  (org-agenda-overriding-header "Important tasks without a date\n")))
      (agenda "" ((org-agenda-span 1)
		  (org-deadline-warning-days 0)
		  (org-agenda-block-separator t)
		  (org-scheduled-past-days 0)
		  ;; We don't need the `org-agenda-date-today'
		  ;; highlight because that only has a practical
		  ;; utility in multi-day views.
		  (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
		  (org-agenda-format-date "%A %-e %B %Y")
		  (org-agenda-overriding-header "\nToday's agenda\n")))
      (agenda "" ((org-agenda-start-on-weekday nil)
		  (org-agenda-start-day "+1d")
		  (org-agenda-span 3)
		  (org-deadline-warning-days 0)
		  (org-agenda-block-separator nil)
		  (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
		  (org-agenda-overriding-header "\nNext three days\n")))
      (agenda "" ((org-agenda-time-grid nil)
		  (org-agenda-start-on-weekday nil)
		  ;; We don't want to replicate the previous section's
		  ;; three days, so we start counting from the day after.
		  (org-agenda-start-day "+4d")
		  (org-agenda-span 14)
		  (org-agenda-show-all-dates nil)
		  (org-deadline-warning-days 0)
		  (org-agenda-block-separator nil)
		  (org-agenda-entry-types '(:deadline))
		  (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
		  (org-agenda-overriding-header "\nUpcoming deadlines (+14d)\n"))))
    "Custom agenda for use in `org-agenda-custom-commands'.")
  :custom
  (org-agenda-files '("~/org/todo.org")); "~/org/contacts.org"))
  (org-agenda-include-diary t)
  (org-agenda-custom-commands
	`(("A" "Daily agenda and top priority tasks"
	   ,lg/org-custom-daily-agenda)))
  :config
  (transient-define-prefix lg/org-agenda-transient ()
    "Replace the org-agenda buffer by a transient."
    [["Built-in agendas"
      ("a" "Current day/week" (lambda () (interactive) (org-agenda nil "a")))
      ("t" "Global todo list" (lambda () (interactive) (org-agenda nil "t")))
      ("T" "Global todo list + choose" (lambda () (interactive) (org-agenda nil "T")))
      ("m" "Search tags" (lambda () (interactive) (org-agenda nil "m")))
      ("M" "Search tags with TODO" (lambda () (interactive) (org-agenda nil "M")))
      ("e" "Export" (lambda () (interactive) (org-agenda nil "e")))
      ("s" "Search" (lambda () (interactive) (org-agenda nil "s")))
      ("S" "Search with TODO" (lambda () (interactive) (org-agenda nil "S")))
      ("/" "Multi-occur" (lambda () (interactive) (org-agenda nil "/")))
      ("<" "Restrict" (lambda () (interactive) (org-agenda nil "<")))
      (">" "Remove restiction" (lambda () (interactive) (org-agenda nil ">")))
      ("#" "List stuck projects" (lambda () (interactive) (org-agenda nil "#")))
      ("!" "Define \"stuck\"" (lambda () (interactive) (org-agenda nil "!")))
      ("C" "Configure custom agenda views" (lambda () (interactive) (org-agenda nil "C")))]
     ["Custom agendas"
      ("A" "Daily and overview" (lambda () (interactive) (org-agenda nil "A")))
      ("H" "Habits tracker" (lambda () (interactive) (org-agenda nil "H")))]]))
	  ;; ("H" "Habits tracker"
	  ;; ,lg/org-agenda-habits-tracker))))

(use-package org-habit
  :straight nil
  :after org
  :init
  (defvar lg/org-agenda-habits-tracker
    `((agenda ""
	      ((org-habit-show-habits t)
	       (org-habit-show-habits-only-for-today nil)
	       (org-habit-following-days 7)
	       (org-habit-preceding-days 14)
	       (org-habit-graph-column 60)
	       (org-agenda-span 'month)
	       (org-agenda-include-diary nil)
	       (org-agenda-use-time-grid nil)
	       (org-agenda-time-grid nil)
	       (org-agenda-show-log t)
	       (org-agenda-remove-tags t)
	       (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp "habitudes"))
	       (org-agenda-overriding-header "Habits tracker"))))
    "An habits tracker view.")
  (add-to-list 'org-agenda-custom-commands 
	       `("H" "Habits tracker"
		 ,lg/org-agenda-habits-tracker))
  :custom
  (org-log-into-drawer "LOGBOOK" "Don't clutter the notes.")
  (org-agenda-show-future-repeats 'next "More legible repeated tasks.")
  (org-habit-show-habits nil "Only show habits in the habits-tracker."))

(use-package org-journal
  :straight t
  :commands (org-journal-new-entry)
  :custom
  (org-journal-dir "~/org/journal/")
  (org-journal-file-type 'monthly)
  (org-journal-time-format "%H:%M"))

;;; Org-babel
(use-package ob-emacs-lisp
  :defer t
  :after org-plus-contrib
  :commands (org-babel-execute:emacs-lisp))

(use-package ob-python
  :commands (org-babel-execute:python)
  :custom (org-babel-python-command "python3"))

(use-package ob-dot
  :defer t
  :after org-plus-contrib
  :commands (org-babel-execute:dot))

;;; UI
(use-package org-sidebar
  :after org 
  :commands (org-sidebar org-sidebar-toggle)
  :straight t)

(use-package org-tree-slide
  :straight t
  :defer t
  :custom
  (org-tree-slide-breadcrumbs nil)
  (org-tree-slide-header nil)
  (org-tree-slide-slide-in-effect nil)
  (org-tree-slide-heading-emphasis nil)
  (org-tree-slide-cursor-init t)
  (org-tree-slide-modeline-display nil)
  (org-tree-slide-skip-done nil)
  (org-tree-slide-skip-comments t)
  (org-tree-slide-fold-subtrees-skipped t)
  (org-tree-slide-skip-outline-level 2)
  (org-tree-slide-never-touch-face t)
  (org-tree-slide-activate-message
   (propertize "Presentation mode ON" 'face 'success))
  (org-tree-slide-deactivate-message
        (propertize "Presentation mode OFF" 'face 'error))
  :init
  ;; inspired by protesilaos' configuration
  (define-minor-mode lg/org-presentation-mode
    "Parameters for plain text presentations with `org-mode'."
    :init-value nil
    :global nil
    (if lg/org-presentation-mode
        (progn
          (unless (eq major-mode 'org-mode)
            (user-error "Not in an Org buffer"))
          (org-tree-slide-mode 1)
          (setq-local display-line-numbers nil))
      (org-tree-slide-mode -1)))
  (general-define-key
   :states '(normal)
   :keymaps 'org-tree-slide-mode-map
   "C-h" #'org-tree-slide-display-header-toggle
   "C-l" #'org-tree-slide-display-header-toggle
   "C-j" #'org-tree-slide-move-next-tree
   "C-k" #'org-tree-slide-move-previous-tree))

;;; Notes
(use-package org-noter
  :straight t
  :defer 20
  :custom
  (org-noter-notes-search-path '("~/org/roam/reference/")); "~/org" ))
  (org-noter-auto-save-last-location t)
  (org-noter-default-notes-file-names '("~/org/lecture.org")) ;;'("lecture.org")
  (org-noter-separate-notes-from-heading t)
  (org-noter-always-create-frame nil)
  (org-noter-kill-frame-at-session-end nil)
  :general
  (:states '(normal visual)
	   :keymaps 'pdf-view-mode-map
	   "i" #'org-noter-insert-note))

(use-package org-pdftools
  :straight t
  :hook (org-mode . org-pdftools-setup-link))

(use-package org-noter-pdftools
  :after org-noter
  :straight t)

;; exporting
(use-package ox
  :commands org-export-dispatch
  :custom
  (org-export-coding-system 'utf-8)
  (org-export-default-language "fr")
  (org-export-preserve-breaks nil)
  (org-export-with-tags nil)

  :config
  (defun org/get-headline-string-element  (headline backend info)
    (let ((prop-point (next-property-change 0 headline)))
      (if prop-point (plist-get (text-properties-at prop-point headline) :parent))))

  (defun org/ensure-latex-clearpage (headline backend info)
    (when (org-export-derived-backend-p backend 'latex)
      (let ((elmnt (org/get-headline-string-element headline backend info)))
	(when (member "newpage" (org-element-property :tags elmnt))
	  (concat "\\clearpage\n" headline)))))

  (add-to-list 'org-export-filter-headline-functions
	       'org/ensure-latex-clearpage))

(use-package ox-latex
  :after ox
  ;; :ensure-system-package texlive-publishers
  :custom
  (org-latex-pdf-process
   '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
     "bibtex %b"
     "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
     "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  :config
  (add-to-list 'org-latex-classes
	       '("IEEEtran"
		 "\\documentclass{IEEEtran}"
		 ("\\section{%s}" . "\\section*{%s}")
		 ("\\subsection{%s}" . "\\subsection*{%s}")
		 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		 ("\\subsubsubsection{%s}" . "\\subsubsubsection*{%s}"))))

(use-package ox-beamer :after ox)

;;; Capture
(use-package org-capture
  :after org
  :bind ("s-c" . org-capture)
  :custom
  (org-capture-templates
   '(("c" "Contact" entry
      (file "~/org/contacts.org")
      "* %?
:PROPERTIES:
:EMAIL: 
:PHONE:
:ALIAS:
:NICKNAME:
:IGNORE:
:ICON:
:NOTE:
:ADDRESS:
:BIRTHDAY:
:END:")
     ("b" "Books" entry
      (file+headline "~/org/lecture.org" "Inbox")
      "* %?\n%^{AUTEUR}p\n%^{DATE_LECTURE}p")
     ("t" "Todo" entry
      (file+headline org-default-notes-file "Inbox")
      "* %?\n%i\n" :prepend t)
     ("r" "Reunion" entry
      (file "~/org/reunions.org")
      "* %?
%^t
:PROPERTIES:
:LIEU: %^{Où a lieu la réunion ?|Présentiel|Skype|Zoom|Teams}
:PEOPLE: Lucas GRUSS 
:END:
** TODO Préparation de réunion (amont) [/]
** Contexte (amont/pendant)
** Notes (pendant)
** TODO Taches à réaliser (après réunion) [/]")
     ("l" "Link" entry (file+headline "~/org/links.org" "Links")
      "* %a %^g\n %?\n %T\n %i"))))

(use-package org-pomodoro
  :straight t
  :after org
  :defer t
  :commands org-pomodoro
  :custom
  (org-pomodoro-keep-killed-pomodoro-time t)
  (org-pomodoro-length 45 "You can only focus for 45 minutes consecutively")
  (org-pomodoro-short-break-length 10 "Taking breaks help your brain process your work")
  (org-pomodoro-long-break-frequency 3)
  (org-pomodoro-ticking-sound-p nil)
  (org-pomodoro-play-sounds nil)
  (org-pomodoro-short-break-sound "/usr/share/sounds/freedesktop/stereo/complete.oga")
  (org-pomodoro-long-break-sound "/usr/share/sounds/freedesktop/stereo/complete.oga")
  (org-pomodoro-finished-sound "/usr/share/sounds/freedesktop/stereo/complete.oga"))

;;; UI
(use-package org-modern
  :straight t
  :defer t
  :config (global-org-modern-mode +1))

(use-package org-appear
  :straight t
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-autolinks t)
  (org-appear-autosubmarkers t)
  (org-appear-autoentities t))

(use-package org-fragtog
  :straight t
  :ensure-system-package dvipng
  :hook (org-mode . org-fragtog-mode))

(use-package org-variable-pitch
  :disabled t
  :straight t
  :hook (org-mode . org-variable-pitch-minor-mode))

(use-package org-preview-html
  :straight t
  :defer t
  :custom
  (org-preview-html-viewer 'xwidget)
  (org-preview-html-refresh-configuration 'save))

;;; Citations
(use-package oc
  :after org
  :custom
  (org-cite-global-bibliography '("~/bib/references.bib"))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (org-cite-export-processors '((beamer csl)
				(latex csl)
				(html csl)
				(t basic)))
  :config
  (use-package oc-basic)
  (use-package oc-biblatex)
  (use-package oc-natbib)
  (use-package oc-csl))

(use-package citeproc
  :straight t
  :after org
  :defer t
  :ensure-system-package
  (("/usr/share/citation-style-language/locales/" . "sudo apt install citation-style-language-locales")
   ("/usr/share/citation-style-language/styles/" . "sudo apt install citation-style-language-styles"))
  :custom
  (org-cite-csl-styles-dir "/usr/share/citation-style-language/styles/")
  (org-cite-csl-locales-dir "/usr/share/citation-style-language/locales/"))

(use-package citar
  :straight t
  :commands (citar-select-ref citar-open citar-open-notes citar-open-entry)
  :after citeproc
  :custom
  (citar-citeproc-csl-styles-dir org-cite-csl-styles-dir)
  (citar-bibliography "~/bib/references.bib")
  (citar-file-extensions '("pdf" "org" "tex" "md"))
  (citar-notes-paths '("~/org/roam/references/")))

(use-package citar-embark
  :after (citar embark)
  :config (citar-embark-mode +1))

(use-package citar-org
  :after (citar org))

(use-package citar-org-roam
  :straight t
  :after (citar org-roam)
  :diminish 'citar-org-roam-mode
  :custom (citar-org-roam-subdir "references")
	  (citar-org-roam-note-title-template "${author} - ${title}\n#+date: %U\n#+filetags: ${tags}")
  :config
  (defun citar-org-roam--create-capture-note (citekey entry)
    "Open or create org-roam node for CITEKEY and ENTRY."
    ;; adapted from https://jethrokuan.github.io/org-roam-guide/#orgc48eb0d
    (let ((title (citar-format--entry
		  citar-org-roam-note-title-template entry)))
      (org-roam-capture-
       :templates
       '(("r" "reference" plain "%?" :if-new
	  (file+head
	   "%(concat
 (when citar-org-roam-subdir (concat citar-org-roam-subdir \"/\")) \"${citekey}.org\")"
	   "#+title: ${title}\n")
	  :immediate-finish t
	  :unnarrowed t))
       :info (list :citekey citekey)
       :node (org-roam-node-create :title title)
       :props '(:finalize find-file))
      (org-roam-ref-add (concat "[cite:@" citekey "]"))))

  (citar-org-roam-mode))

;;; Notes
(use-package org-roam
  :straight t
  :after org
  :defer t
  :custom (org-roam-directory "~/org/roam")
	  (org-roam-database-connector 'sqlite)
  :config (org-roam-db-autosync-mode +1))

(use-package org-roam-ui
  :straight t
  :after org-roam
  :diminish org-roam-ui-follow-mode
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start t)
  (org-roam-ui-browser-function #'browse-url))
  
;;; denote
(use-package denote
  :disabled t
  :straight t
  :hook
  (find-file . denote-link-buttonize-buffer) ;; if you use Markdown or plain text files
  ;; (add-hook 'dired-mode-hook #'denote-dired-mode) ;; OR if only want it in `denote-dired-directories':
  (dired-mode . #'denote-dired-mode-in-directories)
  :bind (("C-c n j" . #'my-denote-journal) ; our custom command
	 ("C-c n n" . #'denote)
	 ("C-c n N" . #'denote-type)
	 ("C-c n d" . #'denote-date)
	 ("C-c n s" . #'denote-subdirectory)
	 ("C-c n t" . #'denote-template)
	 ;; If you intend to use Denote with a variety of file types, it is
	 ;; easier to bind the link-related commands to the `global-map', as
	 ;; shown here.  Otherwise follow the same pattern for `org-mode-map',
	 ;; `markdown-mode-map', and/or `text-mode-map'.
	 ("C-c n i" . #'denote-link) ; "insert" mnemonic
	 ("C-c n I" . #'denote-link-add-links)
	 ("C-c n b" . #'denote-link-backlinks)
	 ("C-c n f f" . #'denote-link-find-file)
	 ("C-c n f b" . #'denote-link-find-backlink)
	 ;; Note that `denote-rename-file' can work from any context, not just
	 ;; Dired bufffers.  That is why we bind it here to the `global-map'.
	 ("C-c n r" . #'denote-rename-file)
	 ("C-c n R" . #'denote-rename-file-using-front-matter)
	 :map dired-mode-map
	 ("C-c C-d C-i" . #'denote-link-dired-marked-notes)
	 ("C-c C-d C-r" . #'denote-dired-rename-marked-files)
	 ("C-c C-d C-R" . #'denote-dired-rename-marked-files-using-front-matter))
  :custom
  (denote-directory (expand-file-name "notes" user-emacs-directory))
  (denote-known-keywords '("emacs" "mpc" "mhe" "paper"))
  (denote-infer-keywords t)
  (denote-sort-keywords t)
  (denote-file-type nil) ; Org is the default, set others here
  (denote-prompts '(title keywords))
  (denote-date-prompt-use-org-read-date t) ;; Pick dates, where relevant, with Org's advanced interface:
  (denote-allow-multi-word-keywords t) ;; default
  (denote-date-format nil) ; read doc string
  (denote-backlinks-show-context t)
  (denote-templates '((reference . "#+reference:")))

  ;; Also see `denote-link-backlinks-display-buffer-action' which is a bit advanced.

  ;; We use different ways to specify a path for demo purposes.
  (denote-dired-directories
   (list denote-directory
	 (thread-last denote-directory (expand-file-name "attachments"))
	 (expand-file-name denote-directory)))
  ;; Also check the commands `denote-link-after-creating',
  ;; `denote-link-or-create'.  You may want to bind them to keys as well.
  :custom
  ;; Here is a custom, user-level command from one of the examples we
  ;; showed in this manual.  We define it here and add it to a key binding
  ;; below.
  (defun my-denote-journal ()
    "Create an entry tagged 'journal', while prompting for a title."
    (interactive)
    (denote
     (denote--title-prompt)
     '("journal")))

  (with-eval-after-load 'org-capture
    (setq denote-org-capture-specifiers "%l\n%i\n%?")
    (add-to-list 'org-capture-templates
		 '("n" "New note (with denote.el)" plain
		   (file denote-last-path)
		   #'denote-org-capture
		   :no-save t
		   :immediate-finish nil
		   :kill-buffer t
		   :jump-to-captured t))))

(use-package deft
  :straight t
  :commands deft
  :custom (deft-recursive t)
          (deft-extensions '("txt" "tex" "org"))
	  (deft-use-filter-string-for-filename t)
	  (deft-directory "~/org"))

;;; misc
(use-package org-contrib
  :straight t
  :after org)

(use-package org-protocol :after org)

(use-package org-drill
  :disabled t
  :straight t
  :defer t)

(use-package org-yt
  :after org
  :straight (org-yt :type git
		    :host github
		    :repo "TobiasZawada/org-yt"))

(provide 'lg-org)
;;; lg-org.el ends here
