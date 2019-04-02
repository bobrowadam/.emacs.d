;;; Begin initialization
(package-initialize)
;; Load My functions first
(when window-system
  (require 'f)
  (add-to-list 'load-path "~/.emacs.d/my-funcs"))

;; Secrets
;; (use-package my-secrets)

;; Sane defaults
(setq scroll-conservatively 10
      scroll-margin 2)
(setq display-time-day-and-date t)
(setq display-time-default-load-average nil)
(setq shift-select-mode nil)
(display-time)
(menu-bar-mode -1)
(when (window-system)
  (setq confirm-kill-emacs 'yes-or-no-p))

(when window-system
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1))

(global-subword-mode t)
(global-superword-mode -1)
(setq inhibit-startup-message t)
(setq ring-bell-function 'ignore
      visible-bell nil)
(setq-default indent-tabs-mode nil)

;; Lets you use minibuffer while in minibuffer:
(setq enable-recursive-minibuffers t)

;; Lines should be 80 characters widh, not 72
(setq fill-column 80)

(setq
 backup-by-copying t      ; don't clobber symlinks
 backup-directory-alist '(("." . "~/.emacs.d/backups"))     ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)      ; use versioned backups

(global-set-key (kbd "C-`") 'unpop-to-mark-command)
(global-set-key (kbd "M-`") 'jump-to-mark)
(global-set-key (kbd "C-^") #'(lambda () (interactive
                                          (delete-indentation -1))))
(delete-selection-mode 1)
(set-default 'indent-tabs-mode nil)
(global-auto-revert-mode 1)
(auto-compression-mode t)
(defalias 'yes-or-no-p 'y-or-n-p)

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")))

;; Use package setup
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(use-package misc-funcs)
(setq use-package-debug t)
(use-package edit-funcs)
(setq use-package-verbose t)
(setq use-package-compute-statistics t)
;; (setq use-package-always-defer t)
(setq use-package-always-ensure t)
(eval-when-compile
  (require 'use-package))

;; General settings:
(setq locale-coding-system 'utf-8) ; pretty
(set-terminal-coding-system 'utf-8) ; pretty
(set-keyboard-coding-system 'utf-8) ; pretty
(set-selection-coding-system 'utf-8) ; please
(prefer-coding-system 'utf-8) ; with sugar on top
(global-set-key (kbd "C-c o") 'other-frame)
(global-set-key (kbd "C-x 8 l") 'insert-λ) ;;
;; (ffap-bindings) ; This for find-file to act as ffap when cursor is on file path
;; (global-set-key (kbd "C-x C-d") 'dired)

;; Theme and font
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(use-package gruber-darker-theme
  :if (window-system)
  :ensure t
  :init (setq custom-safe-themes t)
  :config
  (set-default-font "Latin Modern Mono 19")
  (add-to-list 'default-frame-alist
               '(font . "Latin Modern Mono 19"))
  ;; (load-theme 'ayu)
  ;; (load-theme 'gruber-darker)
  ;; (load-theme 'wheatgrass)
  (load-theme gruvbox-dark-hard)
  ;; (load-theme 'nimbus)
  (sml/setup)
  (display-battery-mode 1))

;; Run refresh-google-calendar periodically
(require 'midnight)
(midnight-mode)
(add-hook 'midnight-hook 'my/refresh-google-calendar)
;; (midnight-delay-set 'midnight-delay "00:00")

(use-package smart-mode-line
  :defer
  :if (window-system)
  :ensure t)

(global-set-key (kbd "C-x j") 'whitespace-cleanup)
(global-set-key (kbd "M-i") 'imenu)

;; custom file path
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(unless (not (file-exists-p custom-file))
  (load custom-file))

;; delete char and delte word with "C-h" "C-M-h"
(define-key key-translation-map [?\C-h] [?\C-?])
(define-key key-translation-map (kbd "<f1>") (kbd "C-h"))
;; (define-key key-translation-map (kbd "<f9>") (kbd "C-s-h"))
(global-set-key (kbd "M-C-h") 'backward-kill-word)

(if window-system
 (progn (require 'server)
  (unless (server-running-p)
    (server-start))))

(defun unset-electric-indent ()
  (electric-indent-mode -1))

(use-package rainbow-delimiters
  :defer
  :ensure t)

(use-package linum-off
  :if (memq window-system '(mac ns))
  :ensure
  :config
  (global-linum-mode 1))

(use-package paredit
  :defer
  :if (window-system)
  :init
  (defun use-paredit-not-sp ()
    "Use paredit and stop using Smartparens."
    (turn-off-smartparens-mode)
    (paredit-mode 1))
  :ensure t
  :bind (:map paredit-mode-map
              ("M-W" . paredit-copy-as-kill))
  :hook
  ((emacs-lisp-mode cider-repl-mode cider-mode lisp-mode slime-repl-mode) . use-paredit-not-sp))

(use-package smartparens
  :defer
  :if (window-system)
  :ensure t
  :bind (:map smartparens-mode-map
              ("M-(" . sp-wrap-round)
              ("M-s" . sp-unwrap-sexp)
              ("C-)" . sp-forward-slurp-sexp)
              ("C-}" . sp-forward-barf-sexp)
              ("C-{" . sp-backward-barf-sexp)
              ("C-(" . sp-backward-slurp-sexp)
              ("C-'" . sp-rewrap-sexp)
              ("M-S" . sp-split-sexp)
              ("M-J" . sp-join-sexp)
              ("M-W" . sp-copy-sexp))
  :config
  (smartparens-global-mode t)
  (show-smartparens-global-mode t))

(use-package company
  :ensure t
  :config
  (global-company-mode t))

(use-package smex
  :if (window-system)
  :ensure t)

(use-package ivy
  :if (window-system)
  :ensure t
  :config
  (use-package flx
    :ensure t)
  (setq ivy-re-builders-alist
        '((t . ivy--regex-fuzzy)))
  (setq ivy-use-selectable-prompt t)
  (setq ivy-initial-inputs-alist nil)
  (ivy-mode 1))

(use-package counsel
  :if (window-system)
  :ensure t
  :init
  (setq counsel-rg-base-command
      "rg --heading --context-separator \" \" -i -M 120 --line-number --color never %s .")
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-x C-m" . counsel-mark-ring)
         ("C-c C-s C-r" . counsel-rg)
         ("C-c C-s C-s" . swiper)))

(use-package whole-line-or-region
  :ensure t
  :config
  (whole-line-or-region-global-mode t))

;; Javascript
(use-package js2-mode
  :defer
  :ensure t
  :init
  (defun my-load-js2-snippets ()
    (yas-minor-mode 1)
    (yas-load-directory (concat user-emacs-directory "snippets")))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

  (defun my/use-eslint-from-node-modules ()
    (let* ((root (locate-dominating-file
                  (or (buffer-file-name) default-directory)
                  "node_modules"))
           (eslint (and root
                        (expand-file-name "node_modules/eslint/bin/eslint.js"
                                          root))))
      (when (and eslint (file-executable-p eslint))
        (setq-local flycheck-javascript-eslint-executable eslint))))
  :hook ((js2-mode . js2-imenu-extras-mode)
         (js2-mode . js2-mode-hide-warnings-and-errors)
         (js2-mode . electric-indent-mode)
         (js2-mode . yas-minor-mode)
         (js2-mode . my-load-js2-snippets)
         (js2-mode . rainbow-delimiters-mode))
  :bind (:map js2-mode-map
              ("C-<tab>" . js2-indent-bounce)
              ("C-c C-s" . nil))
  :config
  (setq-default js2-auto-indent-p nil)
  (setq-default unset-electric-indent)
  (setq-default js-indent-level 2))

(use-package highlight-indent-guides
  :defer
  :ensure t
  :hook (js2-mode . highlight-indent-guides-mode))

(defun setup-tide-mode ()
  (interactive)
  (when (not (tramp-tramp-file-p (buffer-file-name (current-buffer))))
    (tide-setup)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1)))

(use-package tide
  :if (window-system)
  :defer
  :ensure t
  :after (js2-mode)
  :hook (js2-mode . setup-tide-mode)
  :bind (:map tide-mode-map ("C-c C-t C-r" . tide-rename-symbol))
  :config
  (setq company-tooltip-align-annotations t)
  (setq tide-tsserver-process-environment '("TSS_LOG=-level verbose -file /tmp/tss.log"))
  (setq tide-tsserver-process-environment nil)
  (setq tide-format-options
        '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t :placeOpenBraceOnNewLineForFunctions nil)))

(use-package indium
  :if (window-system)
  :defer
  :config
  (setq indium-client-debug t)
  :ensure t)

;; Magit
(use-package magit
  :if (window-system)
  :ensure t
  :bind ("C-x g" . magit-status)
  ;; :hook (magit-post-refresh-hook . diff-hl-magit-post-refresh)
  :config
  (setq magit-completing-read-function 'ivy-completing-read)
  (global-magit-file-mode 1)
  ;; (magit-define-popup-switch 'magit-push-popup
  ;;   ?t "Follow tags" "--follow-tags")
  ;; (transient-append-suffix 'Arguments 'magit-push
  ;;   '("t" "Follow tags" "--follow-tags"))
  )

(use-package magithub
  :disabled t
  :if (window-system)
  :ensure t
  :after (magit)
  :pin melpa
  :config
  (setq epa-pinentry-mode 'loopback)
  (setq magithub-debug-mode t)
  (setq auth-sources '("~/.authinfo" "~/.authinfo.gpg" "~/.netrc"))
  (magithub-feature-autoinject t))

(use-package forge
  :init
  (setq auth-sources '("~/.authinfo" "~/.authinfo.gpg" "~/.netrc"))
  (setq epa-pinentry-mode 'loopback)
  :defer
  :if (window-system)
  :ensure t)

(global-set-key (kbd "C-c C-k") 'my/kill-to-start-of-line)

(use-package zoom-window
  :defer
  :if (window-system)
  :ensure t
  :init
  (custom-set-variables
   '(zoom-window-mode-line-color "DarkGreen"))
  :bind ("C-x C-z" . zoom-window-zoom))

(use-package dired
  :if (window-system)
  :defer
  :config
  (put 'dired-find-alternate-file 'disabled nil)
  (setq dired-listing-switches "-alh")
  ;; (setq insert-directory-program "gls" dired-use-ls-dired t)
  (use-package dired-x
    ;; :hook (dired-mode . dired-omit-mode)
    ))

(use-package tramp
  :defer
  :if (window-system)
  :init
  (custom-set-variables
   '(tramp-password-prompt-regexp
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
      ".*:\0? *")
     nil (tramp)))
  :config
  ;; to connect via proxy:
  ;; /sshx:<proxy-server-name>|ssh:ubuntu@<server name>|sudo:root@<server-name>:/
  (add-to-list 'tramp-restricted-shell-hosts-alist
               "\\shadow\\'")
  (setq tramp-default-method "ssh"))

(use-package docker-tramp
  :if (window-system)
  :defer
  :ensure t
  :config
  (require 'docker-tramp-compat))

(use-package flycheck
  :defer
  :if (window-system)
  :ensure t
  :hook ((js2-mode . flycheck-mode)
         (flycheck-mode . my/use-eslint-from-node-modules)))

(use-package cider
  :defer
  :if (window-system)
  :ensure t
  :hook (cider-mode . rainbow-delimiters-mode)
  :config (setq cider-prompt-for-symbol nil))


(use-package clj-refactor
  :defer
  :ensure t
  :after cider
  :init
  (defun my-clojure-mode-hook ()
    (clj-refactor-mode 1)
    (yas-minor-mode 1)      ; for adding require/use/import statements
    ;; This choice of keybinding leaves cider-macroexpand-1 unbound
    (cljr-add-keybindings-with-prefix "C-c C-m"))
  :hook (cider-mode . my-clojure-mode-hook))

(use-package which-key
  :if (window-system)
  :ensure t
  :config
  (which-key-mode 1))

(use-package projectile
  :if (memq window-system '(mac ns))
  :init
  (setq projectile-keymap-prefix (kbd "C-c p"))
  (setq projectile-completion-system 'ivy)
  (setq projectile-switch-project-action #'projectile-dired)
  :config
  (projectile-mode +1))

(defun my-run-eshell (&optional arg)
  "Create an interactive Eshell buffer.
 if in project use `projectile-run-eshell"
  (interactive "P")
  (if (projectile-project-p)
      (projectile-run-eshell)
    (eshell arg)))
(global-set-key (kbd "C-c e") 'my-run-eshell)

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :init (setq exec-path-from-shell-arguments '("-l"))
  :config
  (exec-path-from-shell-initialize))

(use-package nodejs-repl
  :defer)

;;;; SCALA
(use-package lsp-mode
  :defer
  :if (window-system)
  :init (setq lsp-prefer-flymake nil))

(use-package lsp-ui
  :if (window-system)
  :ensure t
  :hook (lsp-mode . lsp-ui-mode))

(use-package lsp-scala
  :if (window-system)
  :ensure t
  :after scala-mode
  :demand t
  :hook ((scala-mode . lsp) (scala-mode . hs-minor-mode))
  :init (setq lsp-scala-server-command "/usr/local/bin/metals-emacs"))

(use-package company-lsp
  :defer
  :ensure)

(use-package sbt-mode
  :if (window-system)
  :defer
  :ensure t
  :commands sbt-start sbt-command)

(use-package scala-mode
  :if (window-system)
  :mode "\\.s\\(cala\\|bt\\)$"
  :hook (scala-mode . highlight-indent-guides-mode)
  :init
  (defun sbt-compile ()
    (interactive)
    (sbt-command "compile"))
  :bind
  (:map scala-mode-map
        ("C-c C-b C-c" . sbt-command)
        ("C-c C-b C-b" . sbt-compile)
        ("C-c C-b C-s". sbt-switch-to-active-sbt-buffer))
  :config
  (setq scala-indent:align-forms t
        scala-indent:align-parameters t
        scala-indent:indent-value-expression t
        scala-indent:default-run-on-strategy
        scala-indent:operator-strategy))

;; (use-package ensime
;;   :defer
;;   :if (window-system)
;;   :pin melpa
;;   :hook ((scala-mode . unset-electric-indent))
;;   :bind
;;   (:map sbt-mode-map
;;         ("C-c C-c" . ensime-sbt-send-eol))
;;   :config
;;   (setq ensime-sbt-perform-on-save "compile")
;;   :ensure t)

(use-package org-mode
  :if (window-system)
  :init
  (setq org-tree-slide-header nil)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")))
  (setq org-directory (concat (getenv "HOME") "/Dropbox/orgzly"))
  (setq org-capture-templates
        `(("l" "link-entry" entry (file+headline ,(concat org-directory "/inbox.org") "Entries")
           "* %?\n  %i %a")
          ("t" "entry" entry (file+headline ,(concat org-directory "/inbox.org") "Entries")
           "* %?\n  %i")
          ("T" "reminder" entry (file+headline ,(concat org-directory "/tickler.org") "Reminders")
           "* %?\n  %i")))
  (setq org-agenda-files `(,(concat org-directory "/tickler.org") ,(concat org-directory "/google-calendar.org") ,(concat org-directory "/inbox.org") ,(concat org-directory "/gtd.org")))
  (setq org-agenda-start-on-weekday 0)
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  (setq org-refile-targets `((,(concat org-directory "/inbox.org") :maxlevel . 1)
                             (,(concat org-directory "/gtd.org") :maxlevel . 3)
                             (,(concat org-directory "/tickler.org") :maxlevel . 1)))
  (setq org-archive-location (concat org-directory "/done.org::"))
  (setq org-complete-tags-always-offer-all-agenda-tags t)

  :hook (org-mode . (lambda () (org-bullets-mode 1)))
  :bind
  ("C-c a" . org-agenda)
  ("C-c c" . org-capture)
  ("C-c s" . org-save-all-org-buffers))

;; Couldn't bind this in 'use-package' form:
(with-eval-after-load 'org
  (progn
    (define-key org-mode-map (kbd "M-p") #'org-metaup)
    (define-key org-mode-map (kbd "M-n") #'org-metadown)
    (define-key org-mode-map (kbd "M-F") #'org-shiftright)
    (define-key org-mode-map (kbd "M-B") #'org-shiftleft)
    ;; (define-key org-agenda-mode-map (kbd "M-F") #'org-agenda-do-date-later)
    ;; (define-key org-agenda-mode-map (kbd "M-B") #'org-agenda-do-date-earlier)
    (define-key org-read-date-minibuffer-local-map (kbd "C-b")
      (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-day 1))))
    (define-key org-read-date-minibuffer-local-map (kbd "C-f")
      (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-day 1))))
    ))

(use-package org-bullets
  :defer
  :if (window-system)
  :ensure t)

(use-package org-brain
  :ensure t
  :after org-mode
  :init
  (setq org-brain-path (concat org-directory "/org-brain"))
  :config
  (setq org-id-track-globally t)
  (setq org-id-locations-file "~/.emacs.d/.org-id-locations")
  (push '("b" "Brain" plain (function org-brain-goto-end)
          "* %i%?" :empty-lines 1)
        org-capture-templates)
  (setq org-brain-visualize-default-choices 'all)
  (setq org-brain-title-max-length 12))

(use-package yasnippet
  :defer
  :if (window-system)
  :ensure t
  :diminish yas-minor-mode
  :commands yas-minor-mode
  :bind ("C-c TAB" . yas-expand)
  :config (yas-global-mode)
  (yas-load-directory "/Users/bob/.emacs.d/elpa/yasnippet-classic-snippets-1.0.2/snippets/scala-mode/"))

(use-package restclient
  :init
  (add-to-list 'auto-mode-alist '("\\.client$" . restclient-mode))
  :defer
  :if (window-system)
  :ensure t)

(use-package inf-mongo
  :defer
  :if (window-system)
  :ensure t)

;; Ediff setup
(defmacro csetq (variable value)
  `(funcall (or (get ',variable 'custom-set)
                'set-default)
            ',variable ,value))
(csetq ediff-window-setup-function 'ediff-setup-windows-plain)
(csetq ediff-split-window-function 'split-window-horizontally)
(csetq ediff-diff-options "-w")

(use-package lastpass
  :defer
  :if (window-system)
  :ensure t
  :config
  (setq lastpass-user "adam@bigpanda.io")
  (setq lastpass-multifactor-use-passcode t)
  (lastpass-auth-source-enable))

(use-package goto-chg
  :defer
  :ensure t
  :commands goto-last-change
  ;; complementary to
  ;; C-x r m / C-x r l
  ;; and C-<space> C-<space> / C-u C-<space>
  :bind (("C-." . goto-last-change)
         ("C-," . goto-last-change-reverse)))

(use-package ag
  :defer
  :if (window-system)
  :ensure t)
(use-package ripgrep
  :init
  (setq ripgrep-arguments '("--context-separator \" \"" "--heading"))
  (setq ripgrep-highlight-search t)
  :defer
  :if (window-system)
  :ensure t)
(use-package wgrep-ag
  :defer
  :if (window-system)
  :ensure t)

(use-package yaml-mode
  :defer
  :ensure t)

(use-package eshell
  :defer
  :if (window-system)
  :init
  (defalias 'ffo 'find-file-other-window)
  (defalias 'ff 'find-file)
  (defalias 'status 'magit-status)
  ;; :bind (:map eshell-mode-map ("<tab>" . completion-at-point))
  :config
  (add-hook 'eshell-mode-hook
            (lambda ()
              (local-set-key (kbd "M-r")
                             (lambda ()
                               (interactive)
                               (insert
                                (ivy-read "Eshell history: "
                                          (delete-dups
                                           (ring-elements eshell-history-ring))))))
              (define-key eshell-mode-map (kbd "<tab>") 'completion-at-point)
              (local-set-key (kbd "C-c C-h") 'eshell-list-history))))

(use-package eshell-prompt-extras
  :if (window-system)
  :after eshell
  :ensure t
  :config
  (autoload 'epe-theme-lambda "eshell-prompt-extras")
  (setq eshell-highlight-prompt nil
        eshell-prompt-function 'epe-theme-lambda))

(use-package expand-region
  :defer
  :ensure t
  :bind ("M-#" . er/expand-region))

(use-package ibuffer-projectile
  :if (window-system)
  :defer
  :ensure t
  :hook (ibuffer . ibuffer-projectile-set-filter-groups)
  :bind ("C-x C-b" . ibuffer)
  :config
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (cond
     ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
     ((> (buffer-size) 100000) (format "%7.0fk" (/ (buffer-size) 1000.0)))
     ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
     (t (format "%8d" (buffer-size)))))
  (setq ibuffer-formats
        '((mark modified read-only " "
                (name 18 18 :left :elide)
                " "
                (size-h 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " "
                filename-and-process))))

(use-package flyspell-correct
  :if (window-system)
  :defer
  :bind ("C-M-$" . flyspell-correct-at-point)
  :ensure t)

(use-package flyspell-correct-ivy
  :if (window-system)
  :defer
  :ensure t
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-previous-word-generic)))

(use-package ace-window
  :defer
  :ensure t
  :bind ( "C-x o" . ace-window)
  :config
  (setq aw-scope 'frame)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package ace-jump-mode
  :defer
  :ensure t
  :bind
  ("C-c j" . 'ace-jump-mode )
  ("C-c k" . 'ace-jump-mode-pop-mark))

(use-package rainbow-delimiters
  :defer
  :ensure t
  :config
  (rainbow-delimiters-mode))

;; File functions bindings:
(global-set-key (kbd "C-c f r") 'rename-file)

(use-package rich-minority
  :ensure t
  :init
  (setq rm-whitelist (setq rm-whitelist (mapconcat #'identity '( " Paredit" " Smartparens") "\\|")))
  :config
  (rich-minority-mode 1 ))

(use-package anzu
  :ensure t
  :bind (("C-M-%" . anzu-query-replace-regexp)
         ("M-%" . query-replace)
         ("C-c M-%" . anzu-repl))
  :config
  (global-anzu-mode))

(use-package ansible-vault :ensure t
  :defer
  :config
  (add-to-list 'auto-mode-alist '("/vault$" . yaml-mode))
  (add-hook 'yaml-mode-hook
  (lambda ()
    (and (string= (file-name-base) "vault") (ansible-vault-mode 1)))))

(use-package diff-hl
  :defer
  :ensure t
  :config
  (global-diff-hl-mode))

(setq find-function-C-source-directory "~/source/emacs-26.1/src/")

;; (defun animate-scratch-buffer ()
;;   (run-with-timer 0.5 nil
;;                   #'animate-string "Happy hacking!" 3 1))

;; (add-hook 'after-init-hook
;;           #'animate-scratch-buffer)

(use-package golden-ratio
  :if (window-system)
  :defer
  :ensure t
  :init (defun my/gloden-ratio ()
  "Toggle golden ratio"
  (interactive)
  (if golden-ratio-mode
      (progn (golden-ratio-mode -1)
             (balance-windows))
    (progn (golden-ratio-mode)
           (golden-ratio))))
  :bind ("C-x -" . my/gloden-ratio)
  :config (add-to-list 'golden-ratio-extra-commands 'ace-window))

(use-package itail
  :if (window-system)
  :defer
  :ensure t)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)

(use-package multiple-cursors
  :if (window-system)
  :defer
  :ensure t
  :bind
  (("C->" . 'mc/mark-next-like-this)
   ("C-<" . 'mc/mark-previous-like-this)))
(put 'upcase-region 'disabled nil)

(use-package slime
  :if (window-system)
  :defer
  :ensure t
  :config
  (setq inferior-lisp-program "sbcl")
  (setq slime-contribs '(slime-fancy))
  (load (expand-file-name "~/quicklisp/slime-helper.el")))

(use-package slime-company
  :if (window-system)
  :defer
  :ensure t
  :after slime)

(use-package cargo
  :if (window-system)
  :defer
  :ensure t)

(use-package rust-mode
  :defer
  :if (window-system)
  :ensure t
  :after cargo
  :hook
  (rust-mode . cargo-minor-mode)
  (rust-mode . flycheck-rust-setup)
  (rust-mode . flycheck-mode)
  (rust-mode . highlight-indent-guides-mode)
  (rust-mode . eldoc-mode)
  (rust-mode . racer-mode)
  :bind (:map rust-mode-map ("TAB" . #'company-indent-or-complete-common))
  :config
  (setq rust-format-on-save t))

(use-package racer
  :if (window-system)
  :defer
  :ensure t
  :after rust-mode
  :config (setq racer-rust-src-path "/Users/bob/.rustup/toolchains/stable-x86_64-apple-darwin/lib/rustlib/src/rust/src"))

;; (put 'magit-diff-edit-hunk-commit 'disabled nil)

(use-package kubernetes-tramp
  :if (window-system)
  :defer
  :ensure t)

(use-package kubernetes
  :if (window-system)
  :defer
  :ensure t
  :commands (kubernetes-overview))

(use-package redis
  :if (window-system)
  :defer
  :ensure t)

(use-package eww
  :disabled
  :if (memq window-system '(mac ns))
  :defer
  :bind ("C-c b s" . eww-search-words))

(use-package w3m
  :defer
  :init
  (setq w3m-search-default-engine "google-en")
  :bind
  ("C-c b e" . w3m)
  (:map w3m-mode-map ("F" . w3m-view-next-page))
  :config
  (autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t))

(use-package undo-propose
  :defer
  :ensure t
  :bind ("C-c C-/" . undo-propose))

(use-package request
  :ensure t)

(use-package mu4e
  :load-path "/usr/local/Cellar/mu/1.0_1/share/emacs/site-lisp/mu/mu4e"
  :config
  (setq mu4e-maildir "/Users/bob/Maildir/google/")
  ;; (setq mu4e-maildir-shortcuts
  ;;   '( ("/INBOX"               . ?i)
  ;;      ("/[Gmail].Sent Mail"   . ?s)
  ;;      ("/[Gmail].Trash"       . ?t)
  ;;      ("/[Gmail].All Mail"    . ?a)))
  (setq mu4e-get-mail-command "offlineimap")
  (setq mu4e-sent-messages-behavior 'delete)
  (setq
   mu4e-trash-folder "/google/[Gmail].Trash"
   mu4e-refile-folder "/google/[Gmail].Archive"
   message-send-mail-function   'smtpmail-send-it
   smtpmail-default-smtp-server "smtp.gmail.com"
   smtpmail-smtp-server         "smtp.gmail.com")
  (add-to-list 'mu4e-bookmarks
               (make-mu4e-bookmark
                :name "Inboxs"
                :query "maildir:\"/INBOX"
                :key ?i))
  (setq mu4e-update-interval (* 60 30))

  ;; This allows me to use 'helm' to select mailboxes
  (setq mu4e-completing-read-function 'completing-read)
  ;; Why would I want to leave my message open after I've sent it?
  (setq message-kill-buffer-on-exit t)
  ;; Don't ask for a 'context' upon opening mu4e
  (setq mu4e-context-policy 'pick-first)
  ;; Don't ask to quit... why is this the default?
  (setq mu4e-confirm-quit nil)
  :bind ("C-c m" . mu4e))

(use-package yasnippet-classic-snippets
  :after yasnippet
  :ensure t
  :config (yas-load-directory "/Users/bob/.emacs.d/elpa/yasnippet-classic-snippets-1.0.2"))

(use-package github-review
  :defer
  :init (setq github-review-fetch-top-level-and-review-comments t)
  :ensure t)
