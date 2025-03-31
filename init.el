(elpaca elpaca-use-package
  (elpaca-use-package-mode)
  (setq elpaca-use-package-by-default t))
(elpaca-wait)
(when window-system
    (server-start))
(use-package dash
  :config (defalias '-range '-iota))

(use-package s :ensure (:wait t) :demand t)
(use-package llama
  :ensure (:wait t)
  :commands (## 𝝺)
  :config (defalias '𝝺 'llama))

(use-package bob-utils
  :commands (bob/eat-top-project bob/kill-this-buffer bob/jump-to-shell assocdr bob/with-default-dir)
  :load-path "site-lisp"
  :ensure nil)

(setq custom-file (concat user-emacs-directory "./custom.el"))

(use-package basic-settings
  :demand t
  :load-path "./modules"
  :bind
  ("C-S-s" . isearch-forward-thing-at-point)
  ("C-x j" . whitespace-cleanup)
  ("C-^" . (lambda () (interactive (delete-indentation -1))))
  ("M-C-h" . backward-kill-sexp)
  ("C-x -" . my/gloden-ratio)
  ("C-x f" . recentf-open-files)
  ("C-x k" . bob/kill-this-buffer)
  ("M-SPC" . cycle-spacing)
  ("<s-return>" . toggle-frame-fullscreen))

(use-package isearch
  :ensure nil
  :custom
  (isearch-lazy-count t))

(use-package setup-llm
  :demand t
  :load-path "./modules")

(use-package setup-ediff
  :demand t
  :load-path "~/modules")

(use-package scratch-pop
  :bind ("C-c r" . scratch-pop)
  :config
  (add-hook 'kill-emacs-hook 'scratch-pop-backup-scratches)
  (scratch-pop-restore-scratches 3)
  :custom
  (scratch-pop-initial-major-mode 'fundamental-mode)
  (scratch-pop-backup-directory (concat user-emacs-directory "scratch-pop")))

(use-package consult
  :ensure t
  :custom
  (consult-buffer-filter
   '("\\`\\magit.*\\'"
     "^\\*.**$"
     "^[[:space:]]\\*.**$"
     )
   )
  :init
  (setq consult--tofu-char #x100000
        consult--tofu-range #x00fffe)
  :bind
  ("M-i" . consult-imenu)
  ("M-g r" . consult-ripgrep)
  ("M-y" . consult-yank-from-kill-ring))

(use-package casual)

(use-package re-builder
  :ensure nil
  :bind
  (:map reb-mode-map ("M-i" . casual-re-builder-tmenu)))

(use-package calc
  :ensure nil
  :bind
  (:map calc-mode-map ("M-i" . casual-calc-tmenu)))

(use-package dired
  :commands dired
  :ensure nil
  :custom
  (dired-auto-revert-buffer t)
  (dired-listing-switches "-lah --group-directories-first")
  (dired-use-ls-dired t)
  (delete-by-moving-to-trash t)
  (dired-dwim-target t)
  :config
  (setq insert-directory-program
        (s-replace "\n" "" (s-replace "//" "/" (shell-command-to-string "which gls"))))
  :hook (dired-mode . (lambda () (dired-hide-details-mode 1)))
  :bind
  (:map dired-mode-map ("M-i" . casual-dired-tmenu)))

(use-package dired-sidebar
  :commands (dired-sidebar-toggle-sidebar dired-sidebar-toggle-with-current-directory)
  :after (dired)
  :bind
  ;; "C-u C-x D"
  ("C-x C-d" . dired-sidebar-toggle-sidebar)
  :custom
  (dired-sidebar-width 40)
  (dired-sidebar-subtree-line-prefix "  ")
  (dired-sidebar-theme 'vscode)
  (dired-sidebar-use-term-integration t)
  (dired-sidebar-use-custom-font t)
  :hook
  (dired-sidebar-mode . (lambda ()
                          (unless (file-remote-p default-directory)
                            (auto-revert-mode))))
  :custom-face
  (dired-sidebar-face ((t (:family "Menlo"))))
  ;; :config
  ;; (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  ;; (push 'rotate-windows dired-sidebar-toggle-hidden-commands)
  )

(use-package dired-subtree
  :after (dired)
  :bind (:map dired-mode-map
              ("TAB" . dired-subtree-toggle)))

(use-package all-the-icons-dired
  :if (window-system)
  :custom
  (all-the-icons-dired-monochrome nil)
  :after (dired)
  :config
  (add-to-list 'all-the-icons-extension-icon-alist
               '("roc" all-the-icons-fileicon "elm" :face all-the-icons-blue))
  (add-to-list 'all-the-icons-mode-icon-alist
             '(roc-mode all-the-icons-fileicon "elm" :face all-the-icons-blue))
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package diredfl
  :hook
  (dired-mode . diredfl-mode))

(use-package savehist
  :ensure nil
  :init
  (setq savehist-file (locate-user-emacs-file "savehist"))
  (setq history-length 10000)
  (setq history-delete-duplicates t)
  (setq savehist-save-minibuffer-history t)
  (savehist-mode))

(use-package recentf-mode
  :ensure nil
  :init (recentf-mode))

(use-package markdown-ts-mode)

(use-package ob-js
    :ensure nil
    :custom (org-babel-js-cmd "node"))

(use-package ob-typescript)


(use-package org
  :commands (org-agenda)
  :custom
  (org-directory (expand-file-name "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/"))
  (org-babel-python-command "python3")
  (org-hide-emphasis-markers t)
  (org-pretty-entities nil)
  (org-export-with-toc nil)
  (org-confirm-babel-evaluate nil)
  (org-loop-over-headlines-in-active-region t)
  (calendar-longitude 32.085300)
  (calendar-latitude 34.781769)
  (org-enforce-todo-dependencies t)
  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c)")))
  (org-stuck-projects
   '("+LEVEL=1+PROJECT" ("NEXT" "WAITING") ("@IGNORE" "@REMINDER") ""))
  (org-tags-exclude-from-inheritance '("project"))
  (org-capture-templates
   `(("t" "entry" entry (file ,(concat org-directory "20240104T120451--inbox__project.org")) "* %?\n  %i")))
  (org-refile-targets '((org-agenda-files :maxlevel . 3)))
  (org-refile-use-outline-path 'file)
  (org-deadline-warning-days 1)

  :config
  (defun bob/babel-ansi ()
    (when-let ((beg (org-babel-where-is-src-block-result nil nil)))
      (save-excursion
        (goto-char beg)
        (when (looking-at org-babel-result-regexp)
          (let ((end (org-babel-result-end))
                (ansi-color-context-region nil))
            (ansi-color-apply-on-region beg end))))))
  (setq org-babel-lisp-eval-fn 'sly-eval)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (js . t)
     (shell . t)
     (python . t)
     (lisp . t)
     (verb . t)
     (typescript . t)))
  (add-to-list 'org-src-lang-modes '("ts" . typescript))

  ;; Fix bug in ob-js: https://emacs.stackexchange.com/questions/55690/org-babel-javascript-error
  ;; (setq org-babel-js-function-wrapper
  ;;       "console.log(require('util').inspect(function(){\n%s\n}(), { depth: 100 }))")
  :hook
  (org-babel-after-execute . #'bob/babel-ansi)
  (org-mode . (lambda () (org-superstar-mode 1)))
  (org-mode . (lambda () (visual-line-mode 1)))
  (org-archive . org-save-all-org-buffers)
  (org-after-refile-insert . org-save-all-org-buffers)
  :bind
  ("C-c a" . org-agenda)
  ("C-c l" . org-store-link)
  ("C-c c" . org-capture)
  (:map org-mode-map
        ("M-p" . org-metaup)
        ("M-n" . org-metadown)
        ("C-c S" . org-save-all-org-buffers)
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

(use-package org-agenda
  :commands (org-agenda)
  :after (org)
  :custom
  (org-agenda-span 1)
  (org-agenda-files `(,(format "%sjournal" org-directory)
                      "beorg.org"
                      "linear.org"
                      "20240104T120451--inbox__project.org"
                      "20240103T130349--reminders__project.org"
                      "20240103T130420--tasks__project.org"))
  (org-agenda-custom-commands
        '(("b" tags "+OngoingBugs")
          ("n" "Todo next" ((todo "NEXT")))))
  :bind
  (:map org-agenda-mode-map
        ("M-F" . org-agenda-do-date-later)
        ("M-B" . org-agenda-do-date-earlier))
  :ensure nil
  :config
  (org-super-agenda-mode 1))

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

(use-package org-super-agenda
  :config
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
                 :order 1.1
                 :transformer bob/org-super-agenda-custom-header-format)
          (:name "Do Next"
                 :todo "NEXT"
                 :order 2)
          (:name "Do Later"
                 :todo "TODO"
                 :order 3)
          (:name "Waiting"
                 :todo "WAITING"
                 :order 4)))
  (org-super-agenda-mode 1))

(defun bob/truncate-denote-file-name (file-name hash-map)
  (s-replace-regexp "^.+--" "" file-name))

(use-package denote
  :commands (denote denote-mode denote-open-or-create denote-directory-files)
  :custom
  ;; (denote-open-or-create-fn 'consult-file)
  (denote-directory org-directory)
  (denote-date-prompt-use-org-read-date t)
  (denote-prompts '(title keywords file-type))
  :bind
  ("C-c d d" . denote-open-or-create))

(use-package denote-journal
  :ensure (:repo "protesilaos/denote-journal" :fetcher github :files ("*.el" "*.texi"))
  :bind ("C-c d t" . denote-journal-new-or-existing-entry))

;; (use-package uuid :demand t)
(use-package verb
  :after (org)
  :mode ("\\.org\\'" . org-mode)
  :config
  (use-package uuid :demand t)
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
  (setq verb-parse-json-to-alist nil)
  (setq verb-post-response-hook 'parse-verb-response-to-alist)
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map))

(use-package ghub
  :commands (ghub-post))

(use-package magit
  :custom (magit-process-apply-ansi-colors 'filter)
  :config
  (defun bob/create-github-repo ()
    "Create a new Github repo using the Github API."
    (interactive)
    (let ((repo-name (read-string "Repo name: " (get-dir-name (project-root (project-current)))))
          (repo-description (read-string "Repo description: "))
          (repo-homepage (read-string "Repo homepage: "))
          (is-repo-private (yes-or-no-p "Is Repo private: "))
          (repo-is_template (yes-or-no-p "Is Repo is_template: "))
          (current-branch (magit-get-current-branch)))
      (ghub-post "/user/repos" (list  :name repo-name
                                      :description repo-description
                                      :homepage repo-homepage
                                      :private is-repo-private
                                      :is_template repo-is_template))
      (magit-remote-add "origin" (format "git@github.com:bobrowadam/%s.git" repo-name))
      (magit-run-git-async "push"
                           "-u"
                           "origin"
                           (format "refs/heads/%s:refs/heads/%s"
                                   current-branch
                                   current-branch))))
  (magit-wip-mode 1)
  (transient-append-suffix 'magit-file-dispatch
    "p"
    '("P" "Push" magit-push))
  (transient-append-suffix 'magit-file-dispatch
    "P"
    '("F" "Pull" magit-pull))
  (defun bob/magit-commit-message (message)
    (interactive "sCommit message: ")
    (magit-commit-create `("-am" ,message)))
  (transient-append-suffix 'magit-commit
    "c"
    '("m" "Quick commit using minibuffer for commit message." bob/magit-commit-message))
  (defun bob/magit-buffers ()
    "Jump to a magit buffer."
    (interactive)
    (if-let* ((magit-buffers
               (bob/drop-buffer
                (set-last-magit-buffer-as-first
                 (seq-filter
                  (lambda (b) (or (equal (with-current-buffer b major-mode) 'magit-status-mode)))
                  (mapcar (function buffer-name) (buffer-list))))))
              (magit-buffer (completing-read "Magit: " magit-buffers)))
        (progn
          (setq bob/last-magit-buffer magit-buffer)
          (switch-to-buffer magit-buffer))
      (message "No Magit buffers exists")))
  (defun bob/magit-fetch-and-rebase ()
    "Fetch \"origin/main and\" rebase current branch onto \"main\"."
    (interactive)
    (message "Running 'git fetch origin main:main'")
    (magit-run-git "fetch" "origin" "main:main")
    (magit-rebase-branch "main" nil))
  (transient-append-suffix 'magit-rebase
    "f"
    '("F" "Fetch & Rebase" bob/magit-fetch-and-rebase)))

(use-package abridge-diff
  :demand t
  :after magit
  :config (abridge-diff-mode 1))

(use-package pr-review)
(use-package forge
  :after magit
  :custom
  (forge-status-buffer-default-topic-filters
   (forge--topics-spec
    :type 'topic :active nil :state 'open :order 'newest
    :author "bobrowadam"
    :limit 10))
  :init (setq forge-bug-reference-hooks nil))

(use-package paredit
  :hook (emacs-lisp-mode))

(use-package puni
  :custom
  (puni-confirm-when-delete-unbalanced-active-region nil)
  :config
  (defun puni-rewrap (open-wrapper)
    (interactive (list (char-to-string (read-char "Enter wrapper character: "))))
    (let* ((close-wrapper (cond ((string= open-wrapper "(") ")")
                                ((string= open-wrapper "{") "}")
                                ((string= open-wrapper "[") "]")
                                ((string= open-wrapper "<") ">")
                                (t open-wrapper))))
      (puni-squeeze)
      (insert open-wrapper)
      (save-excursion
        (yank)
        (insert close-wrapper))))
  :bind
  (:map puni-mode-map
        ("M-(" . puni-wrap-round)
        ("M-{" . puni-wrap-curly)
        ("M-[" . puni-wrap-square)
        ("M-s" . puni-splice)
        ("M-S" . puni-split)
        ("M-J" . paredit-join-sexps)
        ("C-)" . puni-slurp-forward)
        ("C-(" . puni-slurp-backward)
        ("C-}" . puni-barf-forward)
        ("C-{" . puni-barf-backward)
        ("C-'" . puni-rewrap))
  :hook (typescript-ts-mode c-ts-mode js-ts-mode minibuffer-mode))

(use-package transient)

(use-package vertico
  :init
  (vertico-mode))

(use-package embark
  :bind
  (("C-." . embark-act)
   ("C-h B" . embark-bindings)))

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package hotfuzz
  :demand t
  :ensure ( :package "hotfuzz"
            ;; Inherited from elpaca-order-functions.
            :depth treeless
            :inherit t
            :protocol https
            ;; Inherited from elpaca-menu-item.
            :files (:defaults)
            :fetcher github
            :repo "axelf4/hotfuzz")
  :config
  ;; https://github.com/axelf4/hotfuzz/issues/1#issuecomment-1907058175:
  (defvar +hotfuzz--is-empty nil)
  (defun +hotfuzz-all-completions--enable-history-a (orig content &rest args)
    "Set a variable needed for showing most recent entries."
    (setq +hotfuzz--is-empty (string-empty-p content))
    (apply orig content args))
  (advice-add #'hotfuzz-all-completions
              :around #'+hotfuzz-all-completions--enable-history-a)
  (defun +hotfuzz--adjust-metadata--enable-history-a (orig metadata)
    "Enable showing most recent entries for empty input."
    (if +hotfuzz--is-empty
        metadata
      (funcall orig metadata)))
  (advice-add #'hotfuzz--adjust-metadata
              :around #'+hotfuzz--adjust-metadata--enable-history-a))

(use-package orderless
  :demand t
  :init
  (setq completion-ignore-case nil)
  (setq read-file-name-completion-ignore-case nil)
  (setq orderless-component-separator " +")
  (setq orderless-matching-styles
        '(orderless-literal orderless-prefixes orderless-flex))
  (setq completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion))))
  (setq completion-styles '(hotfuzz orderless basic)))

(use-package marginalia
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode 1))

(use-package corfu
  :custom
  (tab-always-indent 'complete)
  (completion-cycle-threshold 1)
  (corfu-auto-delay 0.06)
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-separator ?\s)          ;; Orderless field separator
  (corfu-quit-at-boundary 'separator)
  (corfu-quit-no-match 'separator)
  (corfu-preview-current t)    ;; Disable current candidate preview
  (corfu-preselect-first nil)    ;; Disable candidate preselection
  (corfu-on-exact-match 'insert)     ;; Configure handling of exact matches
  (corfu-echo-documentation t) ;; Disable documentation in the echo area
  (corfu-scroll-margin 8)        ;; Use scroll margin
  (corfu-auto-prefix 1)

  :init
  (global-corfu-mode))

(use-package cape
  :init
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-rfc1345)
  (add-hook 'completion-at-point-functions #'cape-history)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

(use-package which-key
  :init (which-key-mode 1))

(use-package jsonrpc
  ;; :ensure (:wait t)
  )

(use-package fnm
  :demand t
  :ensure
  (:fetcher github
            :repo "bobrowadam/fnm.el"
            :branch "main"
            :files ("fnm.el")))

(use-package exec-path-from-shell)
(use-package xref)
(use-package buff-menu :ensure nil)

(use-package project
  :custom
  (project-list-file (format "%sprojects" user-emacs-directory))
  (project-vc-extra-root-markers '("package.json"))
  :config
  (require 'project-extras)
  (setq project-switch-commands
        '((project-find-file "Find file")
          (project-dired "Root Directory" "d")
          (eat-project "Eat" "s")
          (bob/eat-top-project "Eat Top" "S")
          (magit-project-status "Magit" "g")
          (consult-ripgrep "RipGrep" "r")
          (consult-project-buffer "Buffers" "b")
          (browse-current-project "Browse" "B")))
  (unless (project-known-project-roots)
    (message "No project file found, indexing projects")
    (progn
      (project-remember-projects-under user-emacs-directory)
      (project-remember-projects-under "~/source/grain/apps/backend/" t)
      (project-remember-projects-under "~/source/grain/packages/" t)))
  :bind
  ("C-x p C-m"  . project-dired)
  ("C-x p b" . consult-project-buffer)
  ("C-x p w" . bob/switch-to-open-project-buffer))

(defun set-eslint-executable-name ()
  (setq flymake-eslint-executable-name
        (if-let ((local-eslint-path (locate-dominating-file "" "node_modules/.bin/eslint")))
            (format "%snode_modules/.bin/eslint" local-eslint-path)
          "eslint")))

(defun bob/flymake-mode-on ()
  (when (and (buffer-file-name)
             (not (equal (-some-> (project-current) project-root expand-file-name)
                     user-emacs-directory)))
      (flymake-mode 1)))

(use-package flymake
  :hook (emacs-lisp-mode . bob/flymake-mode-on)
  :bind
  (:map flymake-mode-map
        ("C-c ! l" . flymake-show-buffer-diagnostics)
        ("C-c ! n" . flymake-goto-next-error)
        ("C-c ! p" . flymake-goto-prev-error)))

(use-package flymake-eslint
  :after flyamke
  :hook
  (typescript-ts-mode . flymake-eslint-enable)
  (typescript-js-mode . flymake-eslint-enable))

(use-package prettier
  :hook (typescript-ts-mode js2-mode js-ts-mode))

(use-package eglot-booster
  :ensure ( :package "eglot-booster"
            :protocol https
            :files ("eglot-booster.el")
            :fetcher github
            :repo "jdtsmith/eglot-booster"))

;; (unload-feature 'eldoc t)
;; (setq custom-delayed-init-variables '())
;; (defvar global-eldoc-mode nil)

;; (elpaca eldoc
;;   (require 'eldoc)
;;   (global-eldoc-mode))

(use-package eldoc-box
  :after eglot
  :bind (:map eglot-mode-map ("C->" . eldoc-box-help-at-point)))

(use-package eglot
  ;; :ensure (:wait t)
  :after (fnm exec-path-from-shell)
  :commands (eglot eglot-ensure eglot-shutdown-all)
  :custom
  (eglot-events-buffer-config '(:size 0 :format full))
  (eglot-sync-connect 0)
  :config
  (exec-path-from-shell-initialize)
  (eglot-booster-mode)
  (add-to-list 'eglot-server-programs
               `((js2-mode js-mode js-ts-mode typescript-ts-mode typescript-mode tsx-ts-mode)
                 . ("typescript-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs
               `((json-mode)
                 . ("vscode-json-languageserver" "--stdio")))
  (add-to-list 'eglot-server-programs
               '(sql-mode . (eglot-sqls "sqls" "-config" ".sqls-config")))
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
  (add-to-list 'eglot-server-programs '((roc-ts-mode) "roc_language_server"))
  (add-to-list 'eglot-server-programs '((zig-mode) "zls"))

  (cl-defmethod project-root ((project (head eglot-project)))
    (cdr project))
  :bind
  (:map eglot-mode-map
        ("C->" . eldoc-print-current-symbol-info)
        ("C-c C-f" . eglot-format)
        ("C-c C-n" . eglot-rename)
        ("M-n" . forward-paragraph)
        ("M-p" . backward-paragraph)
        ("M-." . xref-find-definitions)
        ("M-?" . xref-find-references)
        ("C-<" . eglot-find-typeDefinition)
        ("C-c C-a" . eglot-code-actions))
  :hook
  ((js2-mode c++-mode c++-ts-mode c-mode c-ts-mode typescript-ts-mode tsx-ts-mode python-mode rust-mode json-mode sql-mode haskell-mode roc-ts-mode) . eglot-ensure)
  (eglot-managed-mode .  (lambda ()
                           (when (or (eq (derived-mode-p major-mode) 'typescript-ts-mode)
                                     (eq (derived-mode-p major-mode) 'js-ts-mode))
                             (set-eslint-executable-name)
                             (flymake-eslint-enable)))))

(use-package dape
  :bind
  ("C-x C-a d" . dape)
  ("C-x C-a b" . dape-breakpoint-toggle)
  :custom
  (dape-info-buffer-window-groups '((dape-info-scope-mode dape-info-watch-mode)))
  :config
  (setq dape-inlay-hints nil)
  (add-to-list 'dape-configs
               `(vscode-ts-js-attach
                 modes (js-mode js-ts-mode typescript-ts-mode)
                 host "localhost"
                 port 8123
                 command "node"
                 ;; command-cwd "~/source/vscode-js-debug/dist/"
                 command-cwd "~/.emacs.d/debug-adapters/js-debug"
                 command-args ("src/dapDebugServer.js")
                 :port bob/get-inspect-port
                 :sourceMaps t
                 :resolveSourceMapLocations ["**/dist/**/*"]
                 :cwd dape-cwd-fn
                 :autoAttachChildProcesses t
                 :type "pwa-node"
                 :request "attach"
                 :outputCapture "console"
                 :sourceMapRenames t
                 :autoAttachChildProcesses t
                 :console "internalConsole"
                 :killBehavior "forceful"))

  (add-to-list 'dape-configs
               `(ts-node-attach
                 modes (js-mode js-ts-mode typescript-ts-mode)
                 host "localhost"
                 port 8123
                 command "node"
                 command-cwd "~/.emacs.d/debug-adapters/js-debug"
                 command-args ("src/dapDebugServer.js")
                 :sourceMaps t
                 :resolveSourceMapLocations ["**","!**/node_modules/**"]
                 :cwd dape-cwd-fn
                 :autoAttachChildProcesses t
                 :type "pwa-node"
                 :request "attach"
                 :outputCapture "console"
                 :sourceMapRenames t
                 :autoAttachChildProcesses t
                 :console "internalConsole"
                 :killBehavior "forceful"))
    ;; To not display info and/or buffers on startup
  (remove-hook 'dape-on-start-hooks 'dape-info)
  (remove-hook 'dape-on-start-hooks 'dape-repl)

  ;; Use n for next etc. in REPL
  (setq dape-repl-use-shorthand t)

  ;; Kill compile buffer on build success
  (add-hook 'dape-compile-compile-hooks 'kill-buffer)
  :hook (dape-active-mode . repeat-mode))

(use-package comment-tags
  :custom
  (comment-tags-keywords '("TODO"
                           "FIXME"
                           "BUG"
                           "HACK"
                           "INFO"
                           "DONE"))
  (comment-tags-require-colon t)
  :hook (prog-mode))

(use-package jest-ts-mode
  :ensure (:package "jest-ts-mode"
                    :fetcher github
                    :branch "main"
                    :repo "bobrowadam/jest-ts-mode"
                    :files ("jest-ts-mode.el"))
  :custom
  (jest-ts/environment-variables '(("IN_MEMORY_DB" . "true"))))

(use-package typescript-mode
  :mode (("\\.ts\\'" . typescript-ts-mode) ("\\.tsx\\'" . tsx-ts-mode))
  :bind
  ("C-c C-b" . npm-run-build)
  :hook
  (typescript-ts-mode . (lambda ()
                          (setq-local electric-pair-pairs (append electric-pair-pairs '((?< . ?>))))
                          (jest-ts-mode 1)))
  :config
  (setq electric-pair-pairs (append electric-pair-pairs '((?' . ?'))))
  (fnm-use)
  (setq typescript-ts-mode-indent-offset 2))

;; (use-package web-mode
;;   :mode
;;   ("\\.jsx$" . web-mode)
;;   ("\\.tsx$" . web-mode))


(use-package zig-ts-mode)

(use-package roc-ts-mode)

(use-package c-ts-mode
  :custom
  (c-ts-mode-indent-offset 4)
  :ensure nil)

(use-package treesit-fold
  :commands (global-treesit-fold-indicators-mode)
  :bind
  (:map treesit-fold-mode-map ("C-=" . treesit-fold-toggle))
  :hook
  (typescript-ts-base-mode . treesit-fold-indicators-mode))

(use-package prog-mode
  :ensure nil
  :hook
  (prog-mode . (lambda () (display-line-numbers-mode 1))))

(use-package electric-pair-mode
  :ensure nil
  :hook
  (prog-mode . (lambda ()
                       (unless (eq major-mode 'emacs-lisp-mode)
                         (electric-pair-local-mode 1))))
  (minibuffer-mode . (lambda () (electric-pair-local-mode 1))))

(use-package display-line-numbers
  :ensure nil
  :hook (prog-mode))

(use-package yasnippet-snippets)

(use-package yasnippet
  :custom
  (yas-wrap-around-region t)
  (yas-also-auto-indent-first-line t)
  :hook
  (prog-mode . yas-minor-mode-on)
  (emacs-lisp-mode . yas-minor-mode-on)
  (text-mode . yas-minor-mode-on)
  :bind (:map yas-minor-mode-map
              ("C-<tab>" . yas-expand))
  :config
  (setq yas-snippet-dirs
        `(,(concat user-emacs-directory "snippets")
          ,yasnippet-snippets-dir))
  (yas-reload-all))

(use-package doom-modeline
  :demand t
  :custom
  (doom-modeline-percent-position nil)
  (doom-modeline-time-icon nil)
  (doom-modeline-time nil)
  (doom-modeline-buffer-encoding nil)

  :config
  (doom-modeline-mode 1))

(use-package iedit
  :bind ("C-;" . iedit-mode))

(use-package doom-themes
  :demand t
  :custom
  (doom-monokai-classic-brighter-comments nil)
  (doom-monokai-classic-comment-bg t)
  :config
  (load-theme 'doom-monokai-classic))

(use-package spacious-padding
  :demand t
  :config (spacious-padding-mode 1))

(use-package rg
  :bind
  ("M-g d" . rg))

(use-package wgrep)

(defun bob/monorepo-root ()
  "Finds the topmost root in a multi-project structure."
  (or (-some--> (project-current nil (file-name-parent-directory default-directory))
        project-root
        (let ((default-directory it))
          (bob/monorepo-root)))
      (-some-> (project-current) project-root)))

(defun bob/deadgrep-project ()
  "Open an Eat shell on the highest project"
  (interactive)
  (if-let* ((project--root (bob/monorepo-root)))
      (deadgrep--lookup-override project--root)
    (error "Not in project")))

(use-package deadgrep
  :init
  (setq deadgrep--skip-if-hidden t)
  :custom
  (deadgrep-project-root-function #'bob/deadgrep-project)
  (deadgrep--skip-if-hidden t)
  :bind ("M-g D" . deadgrep))

(use-package csv-mode)

(defun bob/install--grammer-if-missing (language)
  (unless (treesit-language-available-p language)
    (treesit-install-language-grammar language)))

(use-package treesit
  :ensure nil
  :custom
  (treesit-font-lock-level 4)
  :config
  (setq treesit-language-source-alist
        '((bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
          (c . ("https://github.com/tree-sitter/tree-sitter-c"))
          (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
          (css . ("https://github.com/tree-sitter/tree-sitter-css"))
          (cmake . ("https://github.com/uyha/tree-sitter-cmake"))
          (csharp     . ("https://github.com/tree-sitter/tree-sitter-c-sharp.git"))
          (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile"))
          (elisp . ("https://github.com/Wilfred/tree-sitter-elisp"))
          (go . ("https://github.com/tree-sitter/tree-sitter-go"))
          (gomod      . ("https://github.com/camdencheek/tree-sitter-go-mod.git"))
          (html . ("https://github.com/tree-sitter/tree-sitter-html"))
          (java       . ("https://github.com/tree-sitter/tree-sitter-java.git"))
          (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
          (json . ("https://github.com/tree-sitter/tree-sitter-json"))
          (lua . ("https://github.com/Azganoth/tree-sitter-lua"))
          (make . ("https://github.com/alemuller/tree-sitter-make"))
          (markdown . ("https://github.com/MDeiml/tree-sitter-markdown" nil "tree-sitter-markdown/src"))
          (ocaml . ("https://github.com/tree-sitter/tree-sitter-ocaml" nil "ocaml/src"))
          (org . ("https://github.com/milisims/tree-sitter-org"))
          (python . ("https://github.com/tree-sitter/tree-sitter-python"))
          (php . ("https://github.com/tree-sitter/tree-sitter-php"))
          (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src"))
          (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "tsx/src"))
          (ruby . ("https://github.com/tree-sitter/tree-sitter-ruby"))
          (rust . ("https://github.com/tree-sitter/tree-sitter-rust"))
          (sql . ("https://github.com/derekstride/tree-sitter-sql" "gh-pages"))
          (vue . ("https://github.com/merico-dev/tree-sitter-vue"))
          (yaml . ("https://github.com/ikatyang/tree-sitter-yaml"))
          (toml . ("https://github.com/tree-sitter/tree-sitter-toml"))
          (zig . ("https://github.com/maxxnino/tree-sitter-zig")))

        major-mode-remap-alist
        '((c-mode          . c-ts-mode)
          (c++-mode        . c++-ts-mode)
          (c-or-c++-mode   . c-or-c++-ts-mode)
          (cmake-mode      . cmake-ts-mode)
          (conf-toml-mode  . toml-ts-mode)
          (css-mode        . css-ts-mode)
          (js-mode         . js-ts-mode)
          (java-mode       . java-ts-mode)
          (js-json-mode    . json-ts-mode)
          (python-mode     . python-ts-mode)
          ;; (sh-mode         . bash-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (rust-mode       . rust-mode)))

  (add-to-list 'auto-mode-alist '("CMakeLists\\'" . cmake-ts-mode))
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
  (add-to-list 'auto-mode-alist '("\\.y[a]?ml\\'" . yaml-ts-mode))
  (bob/install--grammer-if-missing 'yaml)
  (bob/install--grammer-if-missing 'json)
  (bob/install--grammer-if-missing 'rust)
  (bob/install--grammer-if-missing 'typescript)
  (bob/install--grammer-if-missing 'tsx)
  (bob/install--grammer-if-missing 'javascript)
  (bob/install--grammer-if-missing 'python)
  (bob/install--grammer-if-missing 'c)
  (bob/install--grammer-if-missing 'cpp)
  (bob/install--grammer-if-missing 'zig)
  (bob/install--grammer-if-missing 'bash))

(use-package sh-script
  :ensure nil
  :mode ("\\.sh\\'" . bash-ts-mode)
  :interpreter ("bash" . bash-ts-mode))

(use-package pgmacs
  :ensure (:repo "emarsden/pgmacs" :fetcher github :files ("*.el"))
  :config
  (setenv "POSTGRES_DATABASE" "grain")
  (setenv "POSTGRES_USER" "postgres")
  (setenv "POSTGRES_PASSWORD" "grain"))

(use-package grain-utils
  :commands (grain/run-service debug-migration)
  :ensure nil
  :load-path "modules"
  :bind
  ("C-c C-r" . grain/run-service))

(use-package multiple-cursors
  :bind
  ("C-S-c C-c" . mc/edit-lines)
  ("C-S-c C->" . mc/mark-next-like-this)
  ("C-S-c C-<" . mc/mark-previous-like-this)
  ("C-S-c C-." . mc/mark-all-like-this))

(use-package string-inflection)

(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(bind-key "C-S-p" 'move-line-up)
(bind-key "C-S-n" 'move-line-down)

(use-package highlight-indent-guides
  :custom
  (highlight-indent-guides-auto-even-face-perc 19)
  (highlight-indent-guides-auto-odd-face-perc 11)
  (highlight-indent-guides-auto-top-even-face-perc 30)
  (highlight-indent-guides-auto-top-odd-face-perc 25)
  :config
  (setq highlight-indent-guides-method 'fill
        highlight-indent-guides-responsive 'top
        highlight-indent-guides-auto-enabled t)
  ;; (set-face-foreground 'highlight-indent-guides-character-face "dimgray")
  :hook (prog-mode . highlight-indent-guides-mode))

(use-package ligature
  :demand t
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

(use-package eat
  :commands (eat eat-project bob/eat-top-project)
  :custom
  (eat-term-scrollback-size nil)
  :init
  (setq eat-term-name "xterm-256color")
  :bind
  ("C-!" . eat)
  ("C-c s j" . bob/jump-to-shell)
  ("C-x p s" . eat-project)
  ("C-x p S" . bob/eat-top-project))

(use-package xterm-color
  :config
  (setq compilation-environment '("TERM=xterm-256color"))
  (defun bob/advice-compilation-filter (f proc string)
    (funcall f proc (xterm-color-filter string)))
  (advice-add 'compilation-filter
              :around #'my/advice-compilation-filter))

(use-package elfeed
  :custom
  (elfeed-feeds
   '(("https://learncodethehardway.com/feed.rss" programming learning c)
     ("https://feeds.transistor.fm/thoughts-on-functional-programming-podcast-by-eric-normand" programming lisp)
     ("https://www.reddit.com/r/emacs/.rss" programming emacs reddit)
     ("https://www.reddit.com/r/roc_lang/.rss" programming roc reddit)
     ("https://www.reddit.com/r/planetemacs/.rss" programming emacs reddit)
     ("https://danluu.com/atom.xml" programming blog danluu)
     ("https://protesilaos.com/master.xml" programming blog emacs)
     ("https://eshelyaron.com/rss.xml" programming blog emacs)))
  :bind
  ("C-c w" . elfeed)
  (:map elfeed-search-mode-map
        ("T" . bob/elfeed-search-by-current-entry-tags)
        ("t". bob/elfeed-reset-search-by-current-entry-tags))
  :config
  (defun bob/elfeed-search-by-current-entry-tags ()
    "Display elfeed entries that have the same tags as the entry under the cursor."
    (interactive)
    (elfeed-search-set-filter
     (s-trim (mapconcat
              (lambda (tag)
                (format " +%s" tag))
              (elfeed-entry-tags (elfeed-search-selected :ignore-region))))))

  (defun bob/elfeed-reset-search-by-current-entry-tags ()
    (interactive)
    (elfeed-search-set-filter "+unread")))

(use-package erefactor
  :bind-keymap ("C-c C-v" . erefactor-map)
  :hook (emacs-lisp-mode . erefactor-lazy-highlight-turn-on))

(use-package jinx
  :custom
  (jinx-include-faces '((prog-mode font-lock-variable-name-face
                                   font-lock-comment-face
                                   font-lock-doc-face
                                   font-lock-string-face
                                   git-commit-summary)
                        (conf-mode font-lock-comment-face font-lock-string-face)
                        (yaml-mode . conf-mode)
                        (yaml-ts-mode . conf-mode)))
  :config
  (add-to-list 'jinx-camel-modes 'roc-ts-mode)
  (defun jinx--load-dicts ()
    "Load dictionaries and setup syntax table."
    (setq jinx--dicts (delq nil (mapcar #'jinx--mod-dict
                                        (split-string jinx-languages)))
          jinx--syntax-table (make-syntax-table jinx--base-syntax-table))
    (unless jinx--dicts
      (message "Jinx: No dictionaries available for %S" jinx-languages))
    (dolist (dict jinx--dicts)
      (cl-loop for c across (jinx--mod-wordchars dict) do
               (modify-syntax-entry c "w" jinx--syntax-table)))
    (modify-syntax-entry ?' "." jinx--syntax-table)
    (modify-syntax-entry ?’ "w" jinx--syntax-table)
    (modify-syntax-entry ?. "." jinx--syntax-table))
  :hook (prog-mode . global-jinx-mode)
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages)))

(use-package proced-narrow
  :bind (:map proced-mode-map ("N" . proced-narrow)))

;; ispell-completion-at-point suggestions are too broad so we remove it
;; (add-hook 'text-mode-hook
;;           (lambda ()
;;             (remove-hook 'completion-at-point-functions
;;                          'ispell-completion-at-point t)))
;; (add-hook 'org-mode-hook
;;           (lambda ()
;;             (remove-hook 'completion-at-point-functions
;;                          'ispell-completion-at-point t)))

(use-package avy
  :custom
  (avy-case-fold-search t)
  (avy-timeout-seconds 0.25)
  :bind
  ("C-:" . avy-goto-char-timer)
  (:map isearch-mode-map
        ("C-:" . avy-isearch)))

(use-package ace-window
  :bind
  ( "C-x o" . ace-window)
  ( "M-o" . ace-window)
  :config
  (setq aw-scope 'frame)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package golden-ratio
  :bind
  ("C-x -" . my/gloden-ratio)
  :config
  (add-to-list 'golden-ratio-extra-commands 'ace-window)
  (defun my/gloden-ratio ()
    "Toggle golden ratio"
    (interactive)
    (if golden-ratio-mode
        (progn (golden-ratio-mode -1)
               (balance-windows))
      (progn (golden-ratio-mode)
             (golden-ratio)))))

(use-package breadcrumb
  :hook (prog-mode))

(use-package expand-region :bind ("M-#" . er/expand-region))

(use-package ts-comint
  :custom
  (ts-comint-program-command "ts-node"))

(use-package ts-repl
  :ensure (:repo "nverno/ts-repl" :fetcher github :files ("*.el")))

(use-package kubed)

(use-package combobulate
  :ensure (:repo "mickeynp/combobulate" :fetcher github :files ("*.el"))
  :custom
  (combobulate-key-prefix "C-c o")
  :bind (:map typescript-ts-mode-map ("C-M-SPC" . combobulate-mark-node-dwim))
  :hook (typescript-ts-base-mode))

(use-package macrostep
  :bind (:map emacs-lisp-mode-map ("C-x E" . macrostep-expand)))

(use-package org-linear
  :commands (linear/update-linear-issues)
  :ensure nil
  :load-path "~/source/org-linear/")

(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
