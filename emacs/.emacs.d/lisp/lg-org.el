;; -*- lexical-binding: t; -*-
;;; lg-org: configuration for org
;; Author: Lucas Gruss

;;; Org
(use-package org
  ;;:mode ("\\.org\\'" . org-mode)
  :straight t
  :hook ((org-mode . variable-pitch-mode)
	 (org-mode . auto-fill-mode)
	 (after-save . org-table-recalculate-buffer-tables))
  :init
  (defun org-clocking-buffer () nil) ;; without it, impossible to exit emacs with C-x C-c
  (setq org-directory "~/org/")
  (setq org-agenda-files '("~/org/todo.org" "~/org/contacts.org"))
  (setq org-default-notes-file "~/org/todo.org")
  (setq org-fontify-quote-and-verse-blocks nil)
  (setq org-fontify-whole-heading-line nil)
  (setq org-agenda-include-diary t)
  (setq org-startup-with-latex-preview nil)
  (setq org-hide-leading-stars nil)
  (setq org-startup-indented nil)
  (setq org-archive-location "archive/%s_archive::")
  ;; this is better handled by calling ob-`lang` for the required languages
  ;; (setq org-babel-load-languages
  ;; 	'((emacs-lisp . t)
  ;; 	  (dot . t)))
  :custom
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
  :config 
  (plist-put org-format-latex-options :scale 1.5)
  (when (featurep 'transient)
    (general-define-key
     :states 'normal 
     :keymaps 'org-mode-map
      "<localleader>" 'lg/transient-org)))

;;; org-agenda
(use-package org-agenda
  :after org
  :config
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

  (setq org-agenda-custom-commands
	`(("A" "Daily agenda and top priority tasks"
	   ,lg/org-custom-daily-agenda))))

;;; org-journal
(use-package org-journal
  :straight t
  :commands (org-journal-new-entry)
  :custom
  (org-journal-dir "~/org/journal/")
  (org-journal-file-type 'monthly))

;;; Org-babel
;;;; emacs -lisp
(use-package ob-emacs-lisp
  :defer t
  :after org-plus-contrib
  :commands (org-babel-execute:emacs-lisp))

;;;; python
(use-package ob-python
  :commands (org-babel-execute:python)
  :custom (org-babel-python-command "python3"))

;;;; dot
(use-package ob-dot
  :defer t
  :after org-plus-contrib
  :commands (org-babel-execute:dot))

;;; org-beamer
(use-package ox-beamer
  :defer t
  :after org)

;;; org-indent
(use-package org-indent
  :after org
  :hook (org-mode . org-indent-mode))

;;; Org-sidebar
(use-package org-sidebar
  :after org 
  :commands (org-sidebar org-sidebar-toggle)
  :straight t)

;;; Org-tree-slide
(use-package org-tree-slide
  :straight t
  :after org
  :commands prot/org-presentation-mode
  :init
  (my-leader-def :states 'normal :keymaps 'org-mode-map
    "tP" #'prot/org-presentation-mode)
  :config
  (setq org-tree-slide-breadcrumbs nil)
  (setq org-tree-slide-header nil)
  (setq org-tree-slide-slide-in-effect nil)
  (setq org-tree-slide-heading-emphasis nil)
  (setq org-tree-slide-cursor-init t)
  (setq org-tree-slide-modeline-display nil)
  (setq org-tree-slide-skip-done nil)
  (setq org-tree-slide-skip-comments t)
  (setq org-tree-slide-fold-subtrees-skipped t)
  (setq org-tree-slide-skip-outline-level 2)
  (setq org-tree-slide-never-touch-face t)
  (setq org-tree-slide-activate-message
        (propertize "Presentation mode ON" 'face 'success))
  (setq org-tree-slide-deactivate-message
        (propertize "Presentation mode OFF" 'face 'error))

  (define-minor-mode prot/org-presentation-mode
    "Parameters for plain text presentations with `org-mode'."
    :init-value nil
    :global nil
    (if prot/org-presentation-mode
        (progn
          (unless (eq major-mode 'org-mode)
            (user-error "Not in an Org buffer"))
          (org-tree-slide-mode 1)
          (writeroom-mode 1)
          (org-superstar-mode 1)
          (setq-local display-line-numbers nil)
          (org-indent-mode 1))
      (org-tree-slide-mode -1)
      (writeroom-mode -1)
      (org-superstar-mode -1)
      (setq-local display-line-numbers 'relative)
      (org-indent-mode -1)))

  :bind (("C-c P" . prot/org-presentation-mode)
	 :map org-tree-slide-mode-map
	 ("C-h" . org-tree-slide-display-header-toggle)
	 ("C-l" . org-tree-slide-display-header-toggle)
	 ("C-j" . org-tree-slide-move-next-tree)
	 ("C-k" . org-tree-slide-move-previous-tree)))

;;; Org-noter
(use-package org-noter
  :straight t
  :after org
  :defer t
  :preface
  ;; Allow the user to preempt this and set the document search path
  ;; If not set then use `org-directory'
  (defvar org-noter-notes-search-path nil)
  :config
  (unless org-noter-notes-search-path
    (setq org-noter-notes-search-path (list org-directory)))
  (setq org-noter-auto-save-last-location t
	org-noter-default-notes-file-names '("lecture.org")
	org-noter-separate-notes-from-heading t
	org-noter-always-create-frame nil
	org-noter-kill-frame-at-session-end nil)
  (general-def :states 'normal
    :keymaps 'pdf-view-mode-map
    "i" #'org-noter-insert-note))

;;;; org-pdf-tools
(use-package org-pdftools
  :straight t
  :hook (org-mode . org-pdftools-setup-link))

;;;; org-noter-pdftools
(use-package org-noter-pdftools
  :after org-noter
  :straight t)

;;; Citar
(use-package citar
  :straight t
  :commands (citar-open citar-open-notes citar-open-entry)
  :custom
  (citar-bibliography "~/bib/references.bib")
  (citar-file-extensions '("pdf" "org" "tex"))
  (citar-notes-paths '("~/org/notes/"))
  :config
  (citar-filenotify-setup '(LaTeX-mode-hook org-mode-hook)))

;;;; citar-org
(use-package citar-org
  :after citar)


;;; Ox-report
(use-package ox-report
  :straight t
  :after org
  :defer t)

;;; org-ref
(use-package org-ref
  :straight t
  :disabled t
  :after org
  :defer t)

;;; org-contrib
(use-package org-contrib
  :straight t
  :after org)

;;; org-contacts
(use-package org-contacts
  :after org
  :defer t
  :config
  (setq org-contacts-files '("~/org/contacts.org"))
  ;; (setq org-contacts-keymap)
  (setq org-contacts-matcher "EMAIL<>\"\"|ALIAS<>\"\"|TEL<>\"\"|ADRESSE<>\"\"|ANNIVERSAIRE<>\"\"")
  (setq org-contacts-icon-size 32)
  (setq org-contacts-vcard-file "~/org/contacts.vcf")
  ;; (setq org-contacts-last-update nil)
  (setq org-contacts-tel-property "TEL")
  (setq org-contacts-group-prefix "+")
  (setq org-contacts-icon-property "ICON")
  (setq org-contacts-note-property "NOTE")
  (setq org-contacts-alias-property "ALIAS")
  (setq org-contacts-email-property "EMAIL")
  (setq org-contacts-ignore-property "IGNORE")
  (setq org-contacts-birthday-format "Anniversaire: %l (%Y)")
  (setq org-contacts-address-property "ADRESSE")
  (setq org-contacts-tags-props-prefix "#")
  (setq org-contacts-icon-use-gravatar t)
  (setq org-contacts-enable-completion t)
  (setq org-contacts-birthday-property "ANNIVERSAIRE")
  (setq org-contacts-nickname-property "NICKNAME")
  ;; (setq org-contacts-complete-functions )
  (setq org-contacts-completion-ignore-case t)
  (setq org-contacts-last-read-mail-property "DERNIER_MAIL")
  (setq org-contacts-property-values-separators "[,; \f\11\n\15\13]+")
  (setq org-contacts-email-link-description-format "%s (%d)")
  ;;(setq calendar-date-style 'american)
  (defun org-contacts-anniversaries (&optional field format)
    "Compute FIELD anniversary for each contact, returning FORMAT.
Default FIELD value is \"BIRTHDAY\".

Format is a string matching the following format specification:

  %h - Heading name
  %l - Link to the heading
  %y - Number of year
  %Y - Number of year (ordinal)"
    (let ((calendar-date-style 'american)
          (entry ""))
      (unless format (setq format org-contacts-birthday-format))
      (cl-loop for contact in (org-contacts-filter)
               for anniv = (let ((anniv (cdr (assoc-string
					      (or field org-contacts-birthday-property)
					      (nth 2 contact)))))
			     (when anniv
			       (calendar-gregorian-from-absolute
				(org-time-string-to-absolute anniv))))
               ;; Use `diary-anniversary' to compute anniversary.
               if (and anniv (apply 'diary-anniversary anniv))
               collect (format-spec format
				    `((?l . ,(org-with-point-at (cadr contact) (org-store-link nil)))
				      (?h . ,(car contact))
				      (?y . ,(- (calendar-extract-year date)
						(calendar-extract-year anniv)))
				      (?Y . ,(let ((years (- (calendar-extract-year date)
							     (calendar-extract-year anniv))))
					       (format "%d%s" years (diary-ordinal-suffix years))))))))))

;;; org-protocol
(use-package org-protocol
  :after org)

;;; org-capture
(use-package org-capture
  :after org
  :bind ("s-c" . org-capture)
  :custom
  (org-capture-templates
   '(("t" "Personal todo" entry
      (file+headline org-default-notes-file "Inbox")
      "* %?\n%i\n" :prepend t)
     ("n" "Personal notes" entry
      (file+headline +org-capture-notes-file "Inbox")
      "* %u %?\n%i\n%a" :prepend t)
     ("j" "Journal" entry
      (file+olp+datetree +org-capture-journal-file)
      "* %U %?\n%i\n%a" :prepend t)
     ("p" "Templates for projects")
     ("pt" "Project-local todo" entry
      (file+headline +org-capture-project-todo-file "Inbox")
      "* TODO %?\n%i\n%a" :prepend t)
     ("pn" "Project-local notes" entry
      (file+headline +org-capture-project-notes-file "Inbox")
      "* %U %?\n%i\n%a" :prepend t)
     ("pc" "Project-local changelog" entry
      (file+headline +org-capture-project-changelog-file "Unreleased")
      "* %U %?\n%i\n%a" :prepend t)
     ("o" "Centralized templates for projects")
     ("ot" "Project todo" entry #'+org-capture-central-project-todo-file "* TODO %?\n %i\n %a" :heading "Tasks" :prepend nil)
     ("on" "Project notes" entry #'+org-capture-central-project-notes-file "* %U %?\n %i\n %a" :heading "Notes" :prepend t)
     ("oc" "Project changelog" entry #'+org-capture-central-project-changelog-file "* %U %?\n %i\n %a" :heading "Changelog" :prepend t)
     ("l" "Link" entry (file+headline "~/org/links.org" "Links")
      "* %a %^g\n %?\n %T\n %i"))))

;;; org-pomodoro
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


;;; Integration between org and zotero
;;;; org-zotxt
(use-package org-zotxt
  :after zotxt)

;;;; org-zotxt-noter
(use-package org-zotxt-noter
  :after zotxt)

;;; org-appear
(use-package org-appear
  :straight t
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-autolinks t)
  (org-appear-autosubmarkers t)
  (org-appear-autoentities t))

;;; org-fragtog
(use-package org-fragtog
  :straight t
  :hook (org-mode . org-fragtog-mode))

;;; org-drill
(use-package org-drill
  :straight t
  :defer t)

;;; org-books
(use-package org-books
  :disabled t
  :straight t
  :defer t)

;;; org-variable-pitch
(use-package org-variable-pitch
  :straight t
  :hook (org-mode . org-variable-pitch-minor-mode))

;;; deft
(use-package deft
  :straight t
  :commands deft
  :custom
  (deft-extensions '("txt" "tex" "org"))
  (deft-directory "~/org"))

;;; org-cite
(use-package oc
  :custom
  (org-cite-global-bibliography '("~/bib/references.bib"))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (org-cite-export-processors '((beamer natbib)
				(latex csl)
				(html csl)
				(t basic))))

;;;; export processors
(use-package oc-basic :after org)
(use-package oc-biblatex :after org)
(use-package oc-natbib :after org)
(use-package oc-csl :after org :defer t)
(use-package citeproc
  :straight t
  :defer t
  :ensure-system-package
  (("/usr/share/citation-style-language/locales/" . "apt install citation-style-language-locales")
  ("/usr/share/citation-style-language/styles/" . "apt install citation-style-language-styles"))
  :custom
  (org-cite-csl-styles-dir "/usr/share/citation-style-language/styles/")
  (org-cite-csl-locales-dir "/usr/share/citation-style-language/locales/"))

(provide 'lg-org)
