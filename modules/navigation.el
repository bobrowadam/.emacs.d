(use-package devil)

(use-package consult
  :demand t
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c m" . consult-mode-command)
         ("C-x r b" . consult-bookmark)
         ("C-c k" . consult-kmacro)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)
         ("C-M-#" . consult-register)
         ("M-y" . consult-yank-pop)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-i" . consult-imenu)
         ("M-g i" . consult-imenu-multi)
         ("M-g C-g" . consult-git-grep)
         ("M-g r" . consult-ripgrep)
         ("C-M-s" . consult-line)
         ("C-c M-s e" . consult-isearch)
         :map isearch-mode-map
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi))
  :init
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  (setq consult-narrow-key "<"))

(use-package iserach
  :ensure nil
  :init
  (setq isearch-lazy-count t)
  ;; If you want some flexibility with white-space searching change this var:
  (setq search-whitespace-regexp nil))

(use-package occur
  :ensure nil
  :bind ("C-c o" . occur))

(defun browse-current-project ()
  (interactive)
  (browse-riseup-git-project (project-name (project-current))))

(use-package project-utils
  :demand t
  :ensure nil
  :load-path "./site-lisp")

(defun bob/switch-to-project (dir)
  (interactive (list (funcall project-prompter)))
  (project-switch-project dir)
  (when (read "package.json")
    (bob/update-node-modules-if-needed-sync)))

(defvar Buffer-menu-show-internal nil)
(use-package project
  :after project-utils
  :ensure nil
  :custom
  (project-vc-extra-root-markers '("package.json"))
  :bind
  (("C-x p p" . bob/switch-to-project)
   ("C-x p w" . project-switch-to-open-project)
   ("C-x p b" . bob/project-switch-buffer)
   ("C-x p S" .  bob/eat-top-project)
   ("C-x p m"  . magit-project-status)
   ("C-x p C-m"  . project-dired)
   ("C-x p i" . bob/project-list-buffers)
   ("C-x p h" . #'project-jump-to-rest-client))
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
      (project-remember-projects-under "~/source/" t)
      (project-remember-projects-under "~/source/services" t)
      (project-remember-projects-under "~/source/common-lisp/" t)))
  :config
  (vertico-mode 1)
  (recentf-mode 1)
  ;; (setq project-read-file-name-function #'bobs/project-read-file-name-function)
  ;; (override-project-prompt-project-dir)
)

(use-package savehist
  :ensure nil
  :init
  (setq savehist-file (locate-user-emacs-file "savehist"))
  (setq history-length 10000)
  (setq history-delete-duplicates t)
  (setq savehist-save-minibuffer-history t)
  (savehist-mode))

(use-package recentf-mode :ensure nil)

(use-package fussy
  :disabled t
  :custom
  (fussy-use-cache nil)
  (fussy-score-fn 'flx-score)
  :config
  (push 'fussy completion-styles)
  (setq
   ;; For example, project-find-file uses 'project-files which uses
   ;; substring completion by default. Set to nil to make sure it's using
   ;; flx.
   completion-category-defaults nil
   completion-category-overrides nil)
  (advice-add 'corfu--capf-wrapper :before 'fussy-wipe-cache))

(use-package hotfuzz
  :demand t
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
              :around #'+hotfuzz--adjust-metadata--enable-history-a)
  :straight t)

(use-package orderless
  :after (vertico hotfuzz)
  :init
  (setq completion-ignore-case nil)
  (setq read-file-name-completion-ignore-case nil)
  (setq orderless-component-separator " +")
  (setq orderless-matching-styles
        '(orderless-literal orderless-prefixes orderless-flex))
  (setq completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion))))
  (setq completion-styles '(hotfuzz orderless basic)))

(use-package vertico
  :demand t
  :init
  (setq vertico-scroll-margin 0)
  (setq vertico-count 6)
  (setq vertico-resize nil)
  (setq vertico-cycle t)
  :hook
  (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package marginalia
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)
   ;; ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings))

  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package emacs
  :ensure nil
  :bind ("C-x O" . window-swap-states)
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

(use-package expand-region
  :bind ("M-#" . er/expand-region))

(use-package rg
  ;; :custom
  ;; (rg-command-line-flags '("--multiline"))
  :bind
  ("M-g d" . rg))

(use-package wgrep)

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

(use-package avy
  :init (setq avy-case-fold-search nil)
  :custom
  (avy-timeout-seconds 0.13)
  :bind
  ("C-c M-d" . avy-goto-char-in-line)
  ("C-c M-c" . avy-goto-char-timer))

(use-package ace-window
  :bind
  ( "C-x o" . ace-window)
  ( "M-o" . ace-window)
  :config
  (setq aw-scope 'frame)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package tramp
  :ensure nil
  :init (setq tramp-verbose 1)
  :config
  (setq tramp-password-prompt-regexp
        (concat
         "^.*"
         (regexp-opt
          '("passphrase" "Passphrase"
            ;; English
            "password" "Verification code"
            ;; Deutsch
            "passwort" "Passwort"
            ;; Français
            "mot de passe" "Mot de passe")
          t)
         ".*:\0? *"))
  (setq tramp-default-method "ssh")
  (add-to-list 'tramp-restricted-shell-hosts-alist
               "\\bastion\\'")
  (add-to-list 'tramp-default-proxies-alist
               '("bob$" nil "/sshx:bastion:"))
  (setq remote-file-name-inhibit-locks t
        remote-file-name-inhibit-cache 3600
        tramp-completion-reread-directory-timeout nil
        vc-ignore-dir-regexp (format "\\(%s\\)\\|\\(%s\\)"
                                     vc-ignore-dir-regexp
                                     tramp-file-name-regexp))
  (setq tramp-histfile-override t)
  (add-to-list 'backup-directory-alist
               (cons tramp-file-name-regexp "/tmp/emacs-backup/")))

(use-package docker)
(use-package tramp-container :ensure nil)

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
  (elfeed-search-set-filter "+unread"))

(use-package elfeed
  :custom
  (elfeed-feeds
   '(
     ;; ("https://www.haaretz.co.il/srv/rss---feedly" news politics haaretz)
     ("https://feeds.megaphone.fm/POLTD9269001873" podcast funny rotner)
     ("https://learncodethehardway.com/feed.rss" programming learning c)
     ("https://feeds.resonaterecordings.com/software-unscripted" programming)
     ("https://feeds.transistor.fm/thoughts-on-functional-programming-podcast-by-eric-normand" programming lisp)
     ;; ("https://www.reddit.com/r/listentothis/.rss" music reddit)
     ("https://www.reddit.com/r/emacs/.rss" programming emacs reddit)
     ("https://www.reddit.com/r/roc_lang/.rss" programming roc reddit)
     ("https://www.reddit.com/r/planetemacs/.rss" programming emacs reddit)
     ("http://notarbut.co/feed/podcast" podcast)
     ;; ("https://blog.rust-lang.org/feed.xml" programming rust)
     ;; ("https://www.reddit.com/r/rust/.rss" programming rust reddit)
     ;; ("https://www.reddit.com/r/Clojure/.rss" programming clojure reddit)
     ("https://danluu.com/atom.xml" programming blog danluu)
     ("https://protesilaos.com/master.xml" programming blog emacs)))
  :bind
  ("C-c w" . elfeed)
  (:map elfeed-search-mode-map
        ("T" . bob/elfeed-search-by-current-entry-tags)
        ("t". bob/elfeed-reset-search-by-current-entry-tags)))

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

(use-package p-search
  :commands (p-search)
  :straight (:host github :repo "zkry/p-search"))

(provide 'navigation)
