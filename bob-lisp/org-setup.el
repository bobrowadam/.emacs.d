(use-package org
  :if (window-system)
  :init
  (setq org-loop-over-headlines-in-active-region t)
  (setq calendar-longitude 32.085300)
  (setq calendar-latitude 34.781769)
  (setq org-tree-slide-header nil)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c)")))
  (setq org-directory (concat (getenv "HOME") "/Dropbox/orgzly"))
  (setq org-capture-templates
        `(("l" "link-entry" entry (file+headline ,(concat org-directory "/inbox.org") "Entries")
           "* %?\n  %i %a")
          ("t" "entry" entry (file+headline ,(concat org-directory "/inbox.org") "Entries")
           "* %?\n  %i")
          ("T" "reminder" entry (file+headline ,(concat org-directory "/tickler.org") "Reminders")
           "* %?\n  %i")))
  (setq org-agenda-files
        `(
          ,(concat org-directory "/tickler.org")
          ,(concat org-directory "/riseup-google-calendar.org")
          , (concat org-directory "/private-google-calendar.org")
            ,(concat org-directory "/inbox.org")
            ,(concat org-directory "/gtd.org")
            ,(concat org-directory "/notes.org")
            ,(concat org-directory "/meat-plan.org")))
  
  (setq org-agenda-start-on-weekday 0)
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  (setq org-refile-targets `((,(concat org-directory "/inbox.org") :maxlevel . 1)
                             (,(concat org-directory "/gtd.org") :maxlevel . 3)
                             (,(concat org-directory "/tickler.org") :maxlevel . 1)
                             (,(concat org-directory "/notes.org") :maxlevel . 4)))
  (setq org-archive-location (concat org-directory "/done.org::"))
  (setq org-complete-tags-always-offer-all-agenda-tags t)
  (setq org-stuck-projects
        '("+PROJECT" ("NEXT" "WAITING") ("@IGNORE" "@REMINDER")
          ""))
  (setq org-tags-exclude-from-inheritance '("PROJECT"))
  (setq org-deadline-warning-days 3)
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (js . t)
     (shell . t)))
  (custom-set-faces
   '(org-agenda-current-time ((t (:inherit org-time-grid :foreground "controlAccentColor")))))

  (require 'ob-js)
  ;; Fix bug in ob-js: https://emacs.stackexchange.com/questions/55690/org-babel-javascript-error
  (setq org-babel-js-function-wrapper
        "console.log(require('util').inspect(function(){\n%s\n}(), { depth: 100 }))")
  :hook
  (org-mode . (lambda () (org-bullets-mode 1)))
  (org-archive . org-save-all-org-buffers)
  (org-after-refile-insert . org-save-all-org-buffers)
  )

(use-package request
  ;; :demand t
  :config
  (defun my/refresh-google-calendar ()
    "Refresh google calendar org file."
    (interactive)
    (let ((file-path (concat org-directory "/google-calendar.org"))
          (tmp-path "/tmp/icalawk")
          (ical-private-url (f-read-text "./ical-url-path")))
      (progn
        (f-write-text
         (request-response-data (request
                                 ical-private-url
                                 :parser 'buffer-string
                                 :sync t))
         'utf-8 tmp-path)
        (call-process "ical2org.awk" tmp-path `((:file ,file-path) nil) nil)))))


;; Couldn't bind this in 'use-package' form:
(with-eval-after-load 'org
  (progn
    (define-key org-mode-map (kbd "M-p") #'org-metaup)
    (define-key org-mode-map (kbd "M-n") #'org-metadown)
    (define-key org-mode-map (kbd "M-F") #'org-shiftright)
    (define-key org-mode-map (kbd "M-B") #'org-shiftleft)
    (define-key org-mode-map (kbd "C-c l") #'org-store-link)
    ;; (define-key org-agenda-mode-map (kbd "M-F") #'org-agenda-do-date-later)
    ;; (define-key org-agenda-mode-map (kbd "M-B") #'org-agenda-do-date-earlier)
    (define-key org-read-date-minibuffer-local-map (kbd "C-b")
      (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-day 1))))
    (define-key org-read-date-minibuffer-local-map (kbd "C-f")
      (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-day 1))))
    ))

(use-package org-bullets)

(use-package org-brain
  :init
  (setq org-brain-path (concat org-directory "/brain"))
  :config
  (setq org-id-track-globally t)
  (setq org-id-locations-file "~/.emacs.d/.org-id-locations")
  (push '("b" "Brain" plain (function org-brain-goto-end)
          "* %i%?" :empty-lines 1)
        org-capture-templates)
  (setq org-brain-visualize-default-choices 'all)
  (setq org-brain-title-max-length 12))

(use-package ob-mongo)
(use-package ob-restclient
  :after org
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((restclient . t))))

(provide 'org-setup)
