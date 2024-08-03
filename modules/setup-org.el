(defvar org-directory (expand-file-name "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/"))

(use-package ob-mongo
  :disabled t
  :after org
  :custom
  (ob-mongo:default-mongo-executable "mongo")
  :load-path "~/source/ob-mongo/")


;; took this snippet from:
;; https://emacs.stackexchange.com/questions/44664/apply-ansi-color-escape-sequences-for-org-babel-results
(defun ek/babel-ansi ()
  (when-let ((beg (org-babel-where-is-src-block-result nil nil)))
    (save-excursion
      (goto-char beg)
      (when (looking-at org-babel-result-regexp)
        (let ((end (org-babel-result-end))
              (ansi-color-context-region nil))
          (ansi-color-apply-on-region beg end))))))
(add-hook 'org-babel-after-execute-hook 'ek/babel-ansi)

(use-package org
  :custom
  (org-babel-python-command "python3")
  (org-hide-emphasis-markers t)
  :commands (org-agenda)
  :ensure t
  :if (window-system)
  :init
  (setq org-confirm-babel-evaluate nil)
  (setq org-export-with-toc nil)
  (setq org-pretty-entities nil)
  (setq org-loop-over-headlines-in-active-region t)
  (setq calendar-longitude 32.085300)
  (setq calendar-latitude 34.781769)
  (setq org-tree-slide-header nil)
  (setq org-enforce-todo-dependencies t)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c)")))
  (setq org-stuck-projects
        '("+LEVEL=1+PROJECT" ("NEXT" "WAITING") ("@IGNORE" "@REMINDER") ""))
  ;; +LEVEL=3+boss-TODO​="DONE"
  (setq org-tags-exclude-from-inheritance '("project"))

  (setq org-capture-templates
        `(("t" "entry" entry (file ,(concat org-directory "20240104T120451--inbox__project.org")) "* %?\n  %i")))
  (setq org-refile-targets (setq org-refile-targets '((org-agenda-files :maxlevel . 3))))
  (setq org-refile-use-outline-path 'file)

  (setq org-deadline-warning-days 1)
  :config
  (setq org-babel-lisp-eval-fn 'sly-eval)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (js . t)
     (shell . t)
     ;; (mongo . t)
     (python . t)
     (lisp . t)
     (verb . t)
     (typescript . t)))
  (add-to-list 'org-src-lang-modes '("ts" . typescript))

  (use-package ob-js
    :ensure nil
    :custom (org-babel-js-cmd "node"))
  (use-package ob-typescript
    :ensure nil
     )

  ;; Fix bug in ob-js: https://emacs.stackexchange.com/questions/55690/org-babel-javascript-error
  ;; (setq org-babel-js-function-wrapper
  ;;       "console.log(require('util').inspect(function(){\n%s\n}(), { depth: 100 }))")
  :hook
  (org-mode . (lambda () (org-superstar-mode 1)))
  (org-archive . org-save-all-org-buffers)
  (org-after-refile-insert . org-save-all-org-buffers)
  :bind
  ("C-c l" . org-store-link)
  (:map org-mode-map
        ("M-p" . org-metaup)
        ("M-n" . org-metadown)
        ;; ("C-c c" . org-capture)
        ;; ("C-c S" . org-save-all-org-buffers)

        ("M-," . org-mark-ring-goto)
        ("M-F" . org-shiftright)
        ("M-B" . org-shiftleft)
        ("C-c n R" . org-refile))
  (:map org-read-date-minibuffer-local-map
        ("M-f" . (lambda ()
                   (interactive (org-eval-in-calendar '(calendar-forward-day 1)))))
        ("M-b" . (lambda ()
                   (interactive (org-eval-in-calendar '(calendar-backward-day 1)))))
        ("M-p" . (lambda ()
                   (interactive (org-eval-in-calendar '(calendar-backward-week 1)))))
        ("M-n" . (lambda ()
                   (interactive (org-eval-in-calendar '(calendar-forward-week 1)))))))

(use-package org-habit
  :after (org)
  :ensure nil
  :demand t
  :init (setq org-habit-graph-column 70)
  (add-to-list 'org-modules 'org-habit))

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
      ("WRITING" . (:inherit base-todo-keyword-face :foreground "#C7A941"))))

(use-package org-superstar
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
  (org-superstar-leading-bullet " ")
  (org-superstar-todo-bullet-face-alist tb/org-todo-bullet-faces)
  (org-hide-leading-stars nil))

(defun bob/reset-org-element-cache-in-agenda-files ()
  (dolist (file-path (directory-files (format "%sorg-calendar-agenda" org-directory) t "[^.]"))
    (with-current-buffer (find-file-noselect file-path t)
      (message "Reseting org cache in  %s" (file-name-nondirectory (buffer-file-name (current-buffer))))
      (org-element-cache-reset)
      (save-buffer))))

(use-package org-agenda
  :commands (org-agenda bob/reset-org-element-cache-in-agenda-files)
  :after (org)
  :custom
  (org-agenda-span 1)
  :init
  (setq org-agenda-files `(,(format "%sorg-roam/journal" org-directory)
                           "beorg.org"
                           "20240104T120451--inbox__project.org"
                           "20240103T130349--reminders__project.org"
                           "20240103T130420--tasks__project.org"))

  (setq org-agenda-custom-commands
        '(("b" tags "+OngoingBugs")
          ("n" "Todo next" ((todo "NEXT")))))
  :bind
  (:map org-agenda-mode-map
        ("M-F" . org-agenda-do-date-later)
        ("M-B" . org-agenda-do-date-earlier))
  :ensure nil
  :config
  (org-super-agenda-mode 1))

(use-package org-roam
  :after org
  ;; :after (emacsql-sqlite-builtin)
  :init
  (setq org-roam-graph-viewer "/Applications/Firefox.app/Contents/MacOS/firefox")
  (setq org-roam-v2-ack t)
  (setq org-roam-directory (format "%s/org-roam" org-directory))
  (setq org-id-locations-file (concat org-directory "org-roam/.orgids"))
  (setq org-roam-completion-everywhere t)
  (setq org-roam-dailies-directory "journal/")
  (add-to-list 'display-buffer-alist
               `(,org-directory
                 (display-buffer-in-side-window)
                 (side . right)
                 (slot . 0)
                 (window-width . 0.33)
                 (window-parameters . ((no-other-window . t)
                                       (no-delete-other-windows . t)))))
  :bind
  ;; creates a node if it does not exist, and inserts a link to the node at point:
  ("C-c n i" . org-roam-node-insert)
  ("C-c n t" . org-roam-dailies-goto-today)
  ("C-c n f" . org-roam-node-find)
  ("C-c n c" . org-roam-capture)
  ("C-c n b" . org-roam-buffer-toggle)
  :config
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode))

(use-package org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(use-package setup-org-roam
  :disabled t
  :after (org)
  :demand t
  :ensure nil)

(use-package ox-gfm :after org)

(defun parse-verb-response-to-alist ()
  (when verb-parse-json-to-alist
    (let ((response (slot-value verb-http-response :body)))
      (progn
        (erase-buffer)
        (when response
          (insert (condition-case nil
                      (pp-to-string (json-parse-string response
                                                       :object-type 'alist
                                                       :array-type 'list
                                                       :null-object 'nil))
                    (json-parse-error response))))
        (verb-response-body-mode +1)))))

(defun bob/toggle-verb-parse-json-to-alist ()
  (interactive)
  (setq verb-parse-json-to-alist (not verb-parse-json-to-alist)))

(use-package verb
  :after org
  :mode ("\\.org\\'" . org-mode)
  :init
  (setq verb-parse-json-to-alist nil)
  (setq verb-post-response-hook 'parse-verb-response-to-alist)
  :config
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map))

(use-package ob-typescript)

;; overide denote file prompt
(defun denote-file-prompt (&optional files-matching-regexp)
  "Prompt for file with identifier in variable `denote-directory'.
With optional FILES-MATCHING-REGEXP, filter the candidates per
the given regular expression."
  (let ((files (denote-directory-files files-matching-regexp :omit-current)))
    (completing-read "Select note: " files nil nil nil 'denote-file-history)))

(defun bob/denote-open-or-create (target)
  "See `denote-open-or-create`"
  (interactive (list (bob/completing-read "Select note: "
                                          (denote-directory-files nil :omit-current)
                                          'bob/truncate-denote-file-name
                                          :file-history 'denote--file-history)))
  (if (and target (file-exists-p target))
      (find-file target)
    (denote--command-with-default-title #'denote)))

(defun bob/truncate-denote-file-name (file-name hash-map)
  (s-replace-regexp "^.+--" "" file-name))

(use-package denote
  :commands (denote bob/denote-open-or-create denote-mode denote-open-or-create denote-directory-files)
  :custom
  (denote-directory org-directory)
  (denote-date-prompt-use-org-read-date t)
  (denote-prompts '(title keywords file-type))
  :bind
  ("C-c d" . denote-open-or-create)
)

(use-package consult-notes
  :commands (consult-notes
             consult-notes-search-in-all-notes)
  :config
  ;; If you want to find any headings uncoment this
  ;; (consult-notes-org-headings-mode)
  ;; (consult-notes-denote-mode)
  (consult-notes-org-roam-mode)
  :bind
  ;; ("C-c n f" . consult-notes)
  ;; ("C-c n g" . consult-notes-search-in-all-notes)
)

(use-package org-super-agenda
  :init
  (setq org-super-agenda-groups
        '((:name "Reminders"
                 :file-path "reminders"
                 :order 4)
          (:name "Calendar"
                 :discard (:file-path "reminders")
                 :discard (:tag "chennofar@gmailcom")
                 :time-grid t
                 :order 2)
          (:name "Today Journal"
                 :file-path "journal"
                 :order 1.1)
          (:name "Do Next"
                 :todo "NEXT"
                 :order 2)
          (:name "Do Later"
                 :todo "TODO"
                 :order 3)
          (:name "Waiting"
                 :todo "WAITING"
                 :order 4)))
  :config
  (org-super-agenda-mode 1))

(provide 'setup-org)
