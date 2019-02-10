;;; Begin initialization

(package-initialize)
;; Load My functions first
(when window-system
  (add-to-list 'load-path "~/.emacs.d/my-funcs")
  (use-package misc-funcs)
  (global-set-key (kbd "C-c s j") 'bob/jump-to-eshell)
  (use-package remote-defuns)
  (use-package edit-funcs)
  
  ;; Secrets
  (use-package my-secrets))

;; Sane defaults
(setq scroll-conservatively 10
	scroll-margin 2)
(setq display-time-day-and-date t)
(setq display-time-default-load-average nil)
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
(setq use-package-debug t)
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

;;;;;;  Mark commands ;;;;;; (WIP
;;; Push mark before exprestion jumping
;; (advice-add 'forward-sexp :before (lambda (&rest args) (push-mark (point) t nil)))
;; (advice-add 'backward-sexp :before (lambda (&rest args) (push-mark (point) t nil)))
(global-set-key (kbd "C-`") 'unpop-to-mark-command)
(global-set-key (kbd "M-`") 'jump-to-mark)

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
(eval-when-compile
  (require 'use-package))
(setq use-package-verbose t)
(setq use-package-compute-statistics t)

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
(use-package gruber-darker-theme
  :ensure t
  :after smart-mode-line
  :init (setq custom-safe-themes t)
  :config
  (set-default-font "Latin Modern Mono 19")
  (add-to-list 'default-frame-alist
               '(font . "Latin Modern Mono 19"))
  ;; (load-theme 'ayu)
  ;; (load-theme 'gruber-darker)
  (load-theme 'wheatgrass)
  (sml/setup)
  (display-battery-mode 1))

(use-package smart-mode-line
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
  :defer 3
  :ensure t
  :config
  (global-company-mode t))

(use-package smex
  :if (window-system)
  :ensure t)

(use-package ivy
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
  :defer
  :if (window-system)
  :ensure t
  :init
  (setq counsel-rg-base-command
      "rg -i -M 120 --no-heading --line-number --color never %s .")
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
  :hook (magit-post-refresh-hook . diff-hl-magit-post-refresh)
  :config
  (setq magit-completing-read-function 'ivy-completing-read)
  (magit-define-popup-switch 'magit-push-popup
    ?t "Follow tags" "--follow-tags"))

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
  :if (window-system)
  :ensure t
  :config
  (setq auth-sources '("~/.authinfo" "~/.authinfo.gpg" "~/.netrc"))
  (setq epa-pinentry-mode 'loopback))

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
  :config
  (put 'dired-find-alternate-file 'disabled nil)
  (setq dired-listing-switches "-alh")
  (setq insert-directory-program "gls" dired-use-ls-dired t)
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
  :if (memq window-system '(mac ns))
  :defer
  :if (window-system)
  :after tramp
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
  :defer 2
  :if (window-system)
  :ensure t
  :config
  (which-key-mode 1))

(use-package projectile
  :if (memq window-system '(mac ns))
  :defer 1
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
  :defer 2
  :ensure t
  :if (memq window-system '(mac ns))
  ;; :init (setq exec-path-from-shell-arguments '("-l"))
  :config
  (exec-path-from-shell-initialize))

(use-package nodejs-repl
  :defer
  :ensure t)

(use-package ensime
  :defer
  :if (window-system)
  :pin melpa
  :hook ((scala-mode . unset-electric-indent))
  :bind
  (:map sbt-mode-map
        ("C-c C-c" . ensime-sbt-send-eol))
  :config
  (setq ensime-sbt-perform-on-save "compile")
  :ensure t)

(use-package sbt-mode
  :defer
  :if (window-system)
  :pin melpa)

(use-package scala-mode
  :defer
  :pin melpa
  :hook (scala-mode . highlight-indent-guides-mode)
  :config
  (add-to-list 'auto-mode-alist '("\\.sc$" . scala-mode)))

(use-package org-mode
  :defer 2
  :if (window-system)
  :bind
  ("C-c a" . org-agenda)
  (:map org-mode-map
        ("M-p" . org-metaup)
        ("M-n" . org-metadown)))

(use-package org-projectile
  :if (memq window-system '(mac ns))
  :defer
  :ensure t
  :bind (("C-c n p" . org-projectile-project-todo-completing-read)
         ("C-c n t" . org-projectile-goto-location-for-project)
         ("C-c c" . org-capture))
  :config
  (progn
    (org-projectile-per-project)
    (setq org-projectile-projects-file
          "TODOS.org")
    (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
    (push (org-projectile-project-todo-entry) org-capture-templates)))


(use-package org-bullets
  :if (memq window-system '(mac ns))
  :defer 5
  :if (window-system)
  :ensure t
  :config (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package yasnippet
  :defer
  :if (window-system)
  :ensure t
  :diminish yas-minor-mode
  :commands yas-minor-mode
  :bind ("C-c TAB" . yas-expand)
  :config (yas-global-mode))

(use-package restclient
  :defer
  :if (window-system)
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.client$" . restclient-mode)))

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
  (setq ripgrep-arguments '("-A 2 -B 2" "--context-separator \" \"" "--heading"))
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
  :defer
  :bind ("C-M-$" . flyspell-correct-at-point)
  :ensure t)

(use-package flyspell-correct-ivy
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
  :defer
  :ensure t
  :init
  (setq rm-whitelist (setq rm-whitelist (mapconcat #'identity '( " Paredit" " Smartparens") "\\|")))
  :config
  (rich-minority-mode 1 ))

(use-package anzu
  :if (window-system)
  :defer
  :ensure t
  :bind (("C-M-%" . anzu-query-replace-regexp)
         ("M-%" . query-replace)
         ("C-c M-%" . anzu-repl))
  :config
  (global-anzu-mode +1))

(use-package ansible-vault :ensure t
  :defer
  :config
  (add-to-list 'auto-mode-alist '("/vault$" . yaml-mode))
  (add-hook 'yaml-mode-hook
  (lambda ()
    (and (string= (file-name-base) "vault") (ansible-vault-mode 1)))))

(use-package diff-hl
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
  :ensure t
  :after slime)

(use-package cargo
  :if (window-system)
  :defer 1
  :ensure t)

(use-package rust-mode
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
  :ensure t
  :after rust-mode
  :config (setq racer-rust-src-path "/Users/bob/.rustup/toolchains/stable-x86_64-apple-darwin/lib/rustlib/src/rust/src"))

(put 'magit-diff-edit-hunk-commit 'disabled nil)

(use-package kubernetes-tramp
  :if (window-system)
  :defer
  :ensure t)

(use-package kubernetes
  :if (window-system)
  :ensure t
  :commands (kubernetes-overview))

(use-package redis
  :if (memq window-system '(mac ns))
  :ensure t)

(use-package eww
  :defer
  :bind ("C-c b s" . eww-search-words))
