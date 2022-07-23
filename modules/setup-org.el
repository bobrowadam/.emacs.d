(use-package org
  :demand t
  :ensure nil
  :if (window-system)
  :init
  (setq org-export-with-toc nil)
  (setq org-pretty-entities t)
  (setq org-loop-over-headlines-in-active-region t)
  (setq calendar-longitude 32.085300)
  (setq calendar-latitude 34.781769)
  (setq org-tree-slide-header nil)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c)")))
  (setq org-stuck-projects
        '("+LEVEL=1+PROJECT" ("NEXT" "WAITING") ("@IGNORE" "@REMINDER") ""))
  ;; +LEVEL=3+boss-TODO​="DONE"
  ;; (setq org-tags-exclude-from-inheritance '("PROJECT"))
  (setq org-tags-exclude-from-inheritance nil)
  (setq org-directory (concat (getenv "HOME") "/Dropbox/orgzly"))
  (setq org-capture-templates
        `(("t" "entry" entry (file ,(concat org-directory "/org-roam/20211126120714-inbox.org")) "* %?\n  %i")))
  (setq org-agenda-files
        (list
          ;; ,(concat org-directory "/riseup-google-calendar.org")
          ;; ,(concat org-directory "/private-google-calendar.org")
          (concat org-directory "/org-roam/20211126120714-inbox.org")
          (concat org-directory "/org-roam/20211126182152-tasks.org")
          (concat org-directory "/org-roam/20211126120120-projects.org")
          (concat org-directory "/org-roam/20211126112747-check_this_up.org")
          (concat org-directory "/org-roam/20211126120630-sometime.org")
          (concat org-directory "/org-roam/20211208225633-reminders.org")))
  (setq org-deadline-warning-days 3)
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (js . t)
     (shell . t)))
  (add-to-list 'org-src-lang-modes '("tsx" .t ypescript))
  (custom-set-faces
   '(org-agenda-current-time ((t (:inherit org-time-grid :foreground "controlAccentColor")))))
  (require 'ob-js)

  ;; Fix bug in ob-js: https://emacs.stackexchange.com/questions/55690/org-babel-javascript-error
  ;; (setq org-babel-js-function-wrapper
  ;;       "console.log(require('util').inspect(function(){\n%s\n}(), { depth: 100 }))")
  :hook
  (org-mode . (lambda () (org-superstar-mode 1)))
  (org-archive . org-save-all-org-buffers)
  (org-after-refile-insert . org-save-all-org-buffers)
  :bind
  (:map org-mode-map
        ("M-p" . org-metaup)
        ("M-n" . org-metadown)
        ;; ("C-c c" . org-capture)
        ;; ("C-c S" . org-save-all-org-buffers)
        ("C-c l" . org-store-link)
        ("M-," . org-mark-ring-goto)
        ("M-F" . org-shiftright)
        ("M-B" . org-shiftleft)
        ("C-c n r" . org-roam-refile))
  (:map org-read-date-minibuffer-local-map
        ("M-f" . (lambda () (interactive (org-eval-in-calendar '(calendar-forward-day 1)))))
        ("M-b" . (lambda () (interactive (org-eval-in-calendar '(calendar-backward-day 1)))))
        ("M-p" . (lambda () (interactive (org-eval-in-calendar '(calendar-backward-week 1)))))
        ("M-n" . (lambda () (interactive (org-eval-in-calendar '(calendar-forward-week 1)))))))

(use-package org-bullets
  :if (window-system))

(defvar tb/org-todo-bullet-faces
    '(("NEXT" . (:inherit base-todo-keyword-face :foreground "#FF8580"))
      ("TODO" . (:inherit base-todo-keyword-face :foreground "#CDCD85853F3F"))
      ("ISSUE" . (:inherit base-todo-keyword-face :foreground "#FF8580"
                           :family "github-octicons" :height 160))
      ("BRANCH" . (:inherit base-todo-keyword-face :foreground "#D58422"
                            :family "github-octicons"))
      ("FORK" . (:inherit base-todo-keyword-face :foreground "#D58422"
                            :family "github-octicons"))
      ("MR" . (:inherit base-todo-keyword-face :foreground "#C7A941"
                        :family "github-octicons"))
      ("MERGED" . (:inherit base-todo-keyword-face :foreground "#75AD18"
                            :family "github-octicons"))
      ("GITHUB" . (:inherit base-todo-keyword-face :foreground "#BBBBBB"
                            :family "github-octicons" :height 160))
      ("DONE" . (:inherit base-todo-keyword-face :foreground "#75AD18"))
      ("IDEA" . (:inherit base-todo-keyword-face :foreground "#85AAFF"))
      ("WRITE" . (:inherit base-todo-keyword-face :foreground "#FF8580"))
      ("WRITING" . (:inherit base-todo-keyword-face :foreground "#C7A941"))
          ))

(use-package org-superstar
  ;; :disabled t
  ;; :straight '(org-superstar
  ;;             :fork (:host github
  ;;                          :repo "thibautbenjamin/org-superstar-mode"))
  :after org
  :hook (org-mode . org-superstar-mode)
  :config
  (set-face-attribute 'org-superstar-header-bullet nil :inherit 'fixed-pitched :height 180)
  :custom
  ;; set the leading bullet to be a space. For alignment purposes I use an em-quad space (U+2001)
  (org-superstar-headline-bullets-list '("❍"  9673 9675 10040 10047))
  (org-superstar-todo-bullet-alist '(("DONE" . ?✔)
                                     ("NEXT" . ?☞)
                                     ("TODO" . ?⌖)
                                     ("ISSUE" . ?)
                                     ("BRANCH" . ?)
                                     ("FORK" . ?)
                                     ("MR" . ?)
                                     ("MERGED" . ?)
                                     ("GITHUB" . ?A)
                                     ("WRITING" . ?✍)
                                     ("WRITE" . ?✍)
                                     ))
  (org-superstar-special-todo-items t)
  (org-superstar-leading-bullet "░")
  (org-superstar-todo-bullet-face-alist tb/org-todo-bullet-faces)
  (org-hide-leading-stars nil))

(use-package org-agenda
  :after org
  :bind
  ("C-c a" . org-agenda)
  ("C-c c" . org-capture)
  (:map org-agenda-mode-map
        ("M-F" . org-agenda-do-date-later)
        ("M-B" . org-agenda-do-date-earlier))
  :ensure nil)

(use-package org-roam
  :init
  (setq org-roam-graph-viewer "/Applications/Brave Browser.app/Contents/MacOS/Brave Browser")
  (setq org-roam-v2-ack t)
  (setq org-roam-directory (concat org-directory "/org-roam"))
  (setq org-id-locations-file (concat org-directory "/.orgids"))
  (setq org-roam-completion-everywhere t)
  (setq org-roam-dailies-directory "journal/")
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-side-window)
                 (side . right)
                 (slot . 0)
                 (window-width . 0.33)
                 (window-parameters . ((no-other-window . t)
                                       (no-delete-other-windows . t)))))
  :bind
  ;; creates a node if it does not exist, and inserts a link to the node at point:
  ("C-c n i" . org-roam-node-insert)
  ("C-c n f" . org-roam-node-find)
  ("C-c n c" . org-roam-capture)
  ("C-c n b" . org-roam-buffer-toggle)
  ("C-c n d d" . org-roam-dailies-capture-today)
  :config
  (org-roam-db-autosync-mode))

(use-package org-roam-bibtex
  :after org-roam
  :config
  (require 'org-ref))

(use-package org-roam-ui
  :after org-roam
  ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;;         a hookable mode anymore, you're advised to pick something yourself
  ;;         if you don't care about startup time, use
  ;;  :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(use-package  ob-mongo
  :ensure nil
  :demand t
  :load-path "../ob-mongo"
  :after org)

(use-package ox-gfm :after org)

(provide 'setup-org)
