;;; lg-org: configuration for org
;; Author: Lucas Gruss

;;; Org
(use-package org
  :mode ("\\.org\\'" . org-mode)
  :init
  (defun org-clocking-buffer () nil) ;; without it, impossible to exit emacs with C-x C-c
  (setq org-directory "~/org/")
  (setq org-fontify-quote-and-verse-blocks nil)
  (setq org-fontify-whole-heading-line nil)
  (setq org-agenda-include-diary t)
  (setq org-startup-with-latex-preview t)
  (setq org-hide-leading-stars nil)
  (setq org-startup-indented nil)
  (setq org-archive-location "archive/%s_archive::")
  (setq org-babel-load-languages
	'((emacs-lisp . t)
	  (dot . t)))
  :config
  (setq org-src-block-faces
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
          ("docker" modus-themes-nuanced-cyan))))

;;; Org-sidebar
(use-package org-sidebar
  :after org
  :straight t
  :general
  (my-leader-def :keymaps 'override
    "to" #'org-sidebar-toggle))

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

;;; Ox-report
(use-package ox-report
  :straight t
  :after org
  :defer t)

;;; org-ref
(use-package org-ref
  :straight t
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
  :config
  (setq org-capture-templates
	'(("t" "Personal todo" entry
	   (file+headline +org-capture-todo-file "Inbox")
	   "* [ ] %?\n%i\n%a" :prepend t)
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
  :config
  (setq org-pomodoro-length 45)
  (setq org-pomodoro-short-break-length 10)
  (setq org-pomodoro-long-break-frequency 3)
  (setq org-pomodoro-ticking-sound-p nil)
  (setq org-pomodoro-play-sounds nil)
  (setq org-pomodoro-short-break-sound "/usr/share/sounds/freedesktop/stereo/complete.oga")
  (setq org-pomodoro-long-break-sound "/usr/share/sounds/freedesktop/stereo/complete.oga")
  (setq org-pomodoro-finished-sound "/usr/share/sounds/freedesktop/stereo/complete.oga"))

;(use-package zotxt :straight t)
;(use-package org-zotxt :straight t)
;(use-package org-zotxt-noter :straight t)


;;; visual-fill-column
(defun dw/org-mode-visual-fill ()
  (setq visual-fill-column-width 110
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :defer t
  :straight t
  :hook (org-mode . dw/org-mode-visual-fill))

(provide 'lg-org)
