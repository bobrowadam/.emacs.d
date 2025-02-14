(elpaca elpaca-use-package
  (elpaca-use-package-mode)
  (setq elpaca-use-package-by-default t))

(elpaca-wait)

(setq custom-file (concat user-emacs-directory "./custom.el"))
(defun bob/kill-this-buffer ()
  (interactive)
  (if (one-window-p)
      (kill-buffer)
    (kill-buffer-and-window)))

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

(use-package setup-llm
  :demand t
  :load-path "./modules")

(use-package scratch-pop
  :bind ("C-c r" . scratch-pop)
  :config
  (add-hook 'kill-emacs-hook 'scratch-pop-backup-scratches)
  (scratch-pop-restore-scratches 3)
  :custom
  (scratch-pop-backup-directory (concat user-emacs-directory "scratch-pop")))

(use-package consult
  :ensure t
  :init
  (setq consult--tofu-char #x100000
        consult--tofu-range #x00fffe))

(use-package casual)
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

(use-package dired-subtree
  :after (dired)
  :bind (:map dired-mode-map
              ("TAB" . dired-subtree-toggle)))

(use-package all-the-icons-dired
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

(use-package markdown-ts-mode)

(defvar org-directory (expand-file-name "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/"))
(defun ek/babel-ansi ()
  (when-let ((beg (org-babel-where-is-src-block-result nil nil)))
    (save-excursion
      (goto-char beg)
      (when (looking-at org-babel-result-regexp)
        (let ((end (org-babel-result-end))
              (ansi-color-context-region nil))
          (ansi-color-apply-on-region beg end))))))
(add-hook 'org-babel-after-execute-hook 'ek/babel-ansi)

(use-package ob-js
    :ensure nil
    :custom (org-babel-js-cmd "node"))

(use-package ob-typescript)

(use-package org
  :bind ("C-c a" . org-agenda)
  :ensure ( :package "org"
            :repo ("https://git.savannah.gnu.org/git/emacs/org-mode.git" . "org")
            :pre-build (progn (require 'elpaca-menu-org)
                              (elpaca-menu-org--build))
            :autoloads "org-loaddefs.el"
            :depth 1
            :build (:not elpaca--generate-autoloads-async)
            :files (:defaults ("etc/styles/"
                               "etc/styles/*" "doc/*.texi"))
            :source "Org")
  :custom
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
  :commands (org-agenda)
  :if (window-system)
  :init
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

  

  ;; Fix bug in ob-js: https://emacs.stackexchange.com/questions/55690/org-babel-javascript-error
  ;; (setq org-babel-js-function-wrapper
  ;;       "console.log(require('util').inspect(function(){\n%s\n}(), { depth: 100 }))")
  :hook
  (org-mode . (lambda () (org-superstar-mode 1)))
  (org-mode . (lambda () (visual-line-mode 1)))
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

(use-package org-agenda
  :commands (org-agenda bob/reset-org-element-cache-in-agenda-files)
  :after (org)
  :custom
  (org-agenda-span 1)
  :init
  (setq org-agenda-files `(,(format "%sjournal" org-directory)
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

(defun bob/denote-open-or-create (target)
  "See `denote-open-or-create`"
  (interactive (list (bob/completing-read "Select note: "
                                          (denote-directory-files nil :omit-current)
                                          'bob/truncate-denote-file-name
                                          :file-history 'denote--file-history)))
  (if (and target (file-exists-p target))
      (find-file target)
    (funcall #'denote)))

(defun bob/truncate-denote-file-name (file-name hash-map)
  (s-replace-regexp "^.+--" "" file-name))

(use-package denote
  :commands (denote bob/denote-open-or-create denote-mode denote-open-or-create denote-directory-files)
  :custom
  (denote-open-or-create-fn 'counsel-find-file)
  (denote-directory org-directory)
  (denote-date-prompt-use-org-read-date t)
  (denote-prompts '(title keywords file-type))
  :bind
  ("C-c d d" . denote-open-or-create)
  ("C-c d t" . denote-journal-extras-new-or-existing-entry))

(use-package uuid :demand t)
(use-package verb
  :after (org uuid)
  :mode ("\\.org\\'" . org-mode)
  :config
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
  (require 'uuid)
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map))

(use-package magit
  :config
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
  :bind
  (:map puni-mode-map
        ("M-(" . puni-wrap-round)
        ("M-{" . puni-wrap-curly)
        ("M-<" . puni-wrap-angle)
        ("M-[" . puni-wrap-square)
        ("M-s" . puni-splice)
        ("M-S" . puni-split)
        ("M-J" . paredit-join-sexps)
        ("C-)" . puni-slurp-forward)
        ("C-(" . puni-slurp-backward)
        ("C-}" . puni-barf-forward)
        ("C-{" . puni-barf-backward))
  :hook (typescript-ts-mode c-ts-mode js-ts-mode))

(use-package transient)

(use-package counsel
  :bind
  ("M-i" . counsel-imenu)
  ("C-x b" . #'counsel-switch-buffer))

(use-package vertico
  :init
  (vertico-mode 1))

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
  ;; Optional customizations
  :custom
  (tab-always-indent 'complete)
  (completion-cycle-threshold 1)
  (corfu-auto-delay 0.06)
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-separator ?\s)          ;; Orderless field separator
  (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  (corfu-quit-no-match t)      ;; Never quit, even if there is no match
  (corfu-preview-current t)    ;; Disable current candidate preview
  (corfu-preselect-first nil)    ;; Disable candidate preselection
  (corfu-on-exact-match 'insert)     ;; Configure handling of exact matches
  (corfu-echo-documentation t) ;; Disable documentation in the echo area
  (corfu-scroll-margin 8)        ;; Use scroll margin
  (corfu-auto-prefix 1)

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-excluded-modes'.
  :init
  (global-corfu-mode)
  (defun corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      ;; (setq-local corfu-auto nil) Enable/disable auto completion
      (corfu-mode 1)))

  (defun corfu-send-shell (&rest _)
    "Send completion candidate when inside comint/eshell."
    (cond
     ((and (derived-mode-p 'eshell-mode) (fboundp 'eshell-send-input))
      (eshell-send-input))
     ((and (derived-mode-p 'comint-mode)  (fboundp 'comint-send-input))
      (comint-send-input))))

  (advice-add #'corfu-insert :after #'corfu-send-shell)
  :hook
  (minibuffer-setup . corfu-enable-in-minibuffer))

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-rfc1345))

(use-package which-key
  :init (which-key-mode 1))

(use-package jsonrpc
  :ensure (:wait t) :defer t)

(use-package fnm
  :demand t
  :ensure (:host github
                 :repo "bobrowadam/fnm.el"
                 :branch "main"
                 :files ("fnm.el")))

(use-package exec-path-from-shell)
(use-package xref)
(use-package buff-menu :ensure nil)
(use-package project
  :custom
  (project-vc-extra-root-markers '("package.json"))
  :init
  :init
  (setq project-switch-commands
        '((project-find-file "Find file")
          (project-dired "Root Directory" "d")
          (eat-project "Eat" "s")
          (bob/eat-top-project "Eat Top" "S")
          (magit-project-status "Magit" "g")
          (consult-ripgrep "RipGrep" "r")
          (bob/project-switch-buffer "Buffers" "b")
          (browse-current-project "Browse" "B")))
  (unless (project-known-project-roots)
    (message "No project file found, indexing projects")
    (progn
      (project-remember-projects-under user-emacs-directory)
      (project-remember-projects-under "~/source/grain/apps/backend/" t)
      (project-remember-projects-under "~/source/grain/packages/" t)))
  :bind
  ("C-x p C-m"  . project-dired))

(defun set-eslint-executable-name ()
  (setq flymake-eslint-executable-name
        (format "%snode_modules/.bin/eslint" (locate-dominating-file "" "node_modules"))))

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

(unload-feature 'eldoc t)
(setq custom-delayed-init-variables '())
(defvar global-eldoc-mode nil)

(elpaca eldoc
  (require 'eldoc)
  (global-eldoc-mode))

(use-package eglot
  :ensure (:wait t)
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
        ("C-c C-a" . eglot-code-actions)
        ("C-c ! l" . flymake-show-buffer-diagnostics)
        ("C-c ! n" . flymake-goto-next-error)
        ("C-c ! p" . flymake-goto-prev-error))
  :hook
  ((js2-mode c++-mode c++-ts-mode c-mode c-ts-mode typescript-ts-mode tsx-ts-mode python-mode rust-mode json-mode sql-mode haskell-mode roc-ts-mode) . eglot-ensure)
  (eglot-managed-mode .  (lambda ()
                           (when (or (eq (derived-mode-p major-mode) 'typescript-ts-mode)
                                     (eq (derived-mode-p major-mode) 'js-ts-mode))
                             (set-eslint-executable-name)
                             (flymake-eslint-enable)))))

(use-package jest-ts-mode
  :ensure 
  (:package "jest-ts-mode"
            :fetcher github
            :repo "bobrowadam/jest-ts-mode"
            :files ("jest-ts-mode.el")))

(use-package typescript-mode
  :mode ("\\.ts\\'" . typescript-ts-mode)
  :bind (("C-c C-b" . npm-run-build)
         ("C-c C-r" . npm-run)
         :map typescript-ts-mode-map
         ("C-c C-t C-n" . jest-ts-mode/run-tests)
         ("C-c C-t C-p" . jest-ts-mode/run-test-at-point)
         ("C-c C-t C-r" . jest-ts-mode/rerun-latest-test)
         ("C-c C-t C-j" . jest-ts-mode/jump-to-latest-test))
  :hook
  (typescript-ts-mode . (lambda ()
                          (setq-local electric-pair-pairs (append electric-pair-pairs '((?< . ?>))))))
  :config
  (fnm-use)
  (setq typescript-ts-mode-indent-offset 2))

(use-package roc-ts-mode)

(use-package prog-mode
  :ensure nil
  :hook
  (prog-mode . (lambda () (display-line-numbers-mode 1))))

(use-package electric-pair-mode
  :ensure nil
  :hook (prog-mode))

(use-package display-line-numbers
  :ensure nil
  :hook (prog-mode))

(use-package yasnippet
  :ensure ( :package "yasnippet"
            :repo "joaotavora/yasnippet"
            :fetcher github
            :files ("yasnippet.el" "snippets")
            :source "MELPA")
  :hook (prog-mode . yas-minor-mode-on)
  :bind ("C-<tab>" . yas/expand))

(use-package yasnippet-snippets
  :after yasnippet)

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
  ;; :custom-face
  ;; (font-lock-variable-name-face ((t (:foreground "#59c2ff"))))
  :custom
  (setq doom-pine-padded-modeline t)

  :config
  ;; (doom-themes-set-faces 'user '(default :background "grey8"))
  (load-theme 'doom-pine))

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
          (zig . ("https://github.com/GrayJack/tree-sitter-zig")))

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
          (sh-mode         . bash-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (rust-mode       . rust-mode)))

  (add-to-list 'auto-mode-alist '("CMakeLists\\'" . cmake-ts-mode))
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
  (add-to-list 'auto-mode-alist '("\\.y[a]?ml\\'" . yaml-ts-mode))
  (bob/install--grammer-if-missing 'yaml)
  (bob/install--grammer-if-missing 'rust)
  (bob/install--grammer-if-missing 'typescript)
  (bob/install--grammer-if-missing 'tsx)
  (bob/install--grammer-if-missing 'javascript)
  (bob/install--grammer-if-missing 'python)
  (bob/install--grammer-if-missing 'c)
  (bob/install--grammer-if-missing 'cpp))

(put 'narrow-to-region 'disabled nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("56044c5a9cc45b6ec45c0eb28df100d3f0a576f18eef33ff8ff5d32bac2d9700" default)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
