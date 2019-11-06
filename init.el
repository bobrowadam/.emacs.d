;; (setq debug-on-error t)
(require 'package)
(package-initialize)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(unless (not (file-exists-p custom-file))
  (load custom-file))
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-always-defer t)
(setq use-package-always-ensure t)
(setq use-package-verbose t)
(setq use-package-debug t)
(setq use-package-verbose t)
(setq use-package-compute-statistics t)

(require 'dash)

(require 'dired)
(require 'dired-x)
(setq dired-use-ls-dired nil)
(setq dired-listing-switches "-alh")

(use-package f)
(use-package which-key
  :demand t
  :if (window-system)
  :config
  (which-key-mode 1))
(use-package exec-path-from-shell
  :demand t
  :init (setq exec-path-from-shell-arguments '("-l"))
  :config (exec-path-from-shell-initialize))
(use-package rich-minority
  :demand t
  :init
  (setq rm-whitelist (setq rm-whitelist (mapconcat #'identity '( " Paredit" " Smartparens" " Projectile") "\\|")))
  (rich-minority-mode 1))
(use-package flycheck
  :demand t)
(use-package basic-settings
  :demand t
  :load-path "./bob-lisp"
  :bind
  ("C-c M-s" . isearch-forward-symbol-at-point)
  ("M-i" . imenu)
  ("C-x j" . whitespace-cleanup)
  ("C-^" . (lambda () (interactive (delete-indentation -1))))
  ("M-C-h" . backward-kill-sexp)
  ("C-x -" . my/gloden-ratio)
  ("C-x f" . recentf-open-files)
  ("M-o" . other-frame)
  ("C-x k" . kill-this-buffer))

(use-package setup-eshell
  :demand t
  :load-path "./bob-list"
  :bind
  (:map eshell-mode-map ("M-r" . counsel-esh-history)))

(use-package tramp-settings
  :demand t
  :load-path "./bob-lisp")
(use-package remote-defuns
  :demand t
  :load-path "./bob-lisp"
  :bind
  ("C-c s s". bob/shell)
  ("C-c s j" . bob/jump-to-shell))
(use-package misc-funcs
  :demand t
  :load-path "./bob-listp")
(use-package edit-funcs
  :demand t
  :load-path "./bob-lisp"
  :bind
  ("C-`" . unpop-to-mark-command)
  ("M-`" . jump-to-mark))
(use-package setup-parens
  :load-path "./bob-listp"
  :demand t)
(use-package inf-mongo)
(use-package ibuffer-setup
  :load-path "./bob-lisp"
  :demand t)
(use-package ido-setup
  :disabled t
  :demand t
  :load-path "./bob-lisp")
(use-package setup-ivy
  :demand t
  :load-path "./bob-lisp"
  :bind
  (("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("C-x C-m" . counsel-mark-ring)
   ("C-c C-s C-r" . counsel-rg)
   ("C-c C-s C-s" . swiper)))

(use-package setup-magit
  :bind
  ("C-x g" . magit-status)
  :demand t
  :load-path "./bob-lisp")

(use-package whole-line-or-region
  :demand t
  :init (whole-line-or-region-mode 1))
(use-package company
  :demand t
  :init
  (setq company-tooltip-align-annotations t)
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0.2)
  :config (global-company-mode 1))
(use-package setup-projectile
  :load-path "./bob-lisp"
  :demand t
  :bind
  ("C-c e" . my-run-eshell)
  (:map projectile-mode-map ("C-c p" . projectile-command-map)))
(use-package sicp)
(use-package ace-window
  :bind ( "C-x o" . ace-window)
  :config
  (setq aw-scope 'frame)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))
(use-package highlight-indent-guides
  :demand t
  :hook
  (scala-mode . highlight-indent-guides-mode))

(use-package lsp-setup
  :demand t
  :load-path "./bob-lisp"
  :bind
  (:map lsp-mode-map
        ("C-c C-." . lsp-ui-sideline-toggle-symbols-info)
        ("C-c C-r" . lsp-find-references)
        ("C-c M-i" . lsp-ui-imenu)
        ("C-c M-d" . lsp-describe-thing-at-point)
        ("C-c C-f" . lsp-format-buffer)
        ("C-=" . origami-toggle-node)))

(use-package scala-setup
  :after (company lsp-setup)
  :demand t
  :load-path "./bob-lisp")

(use-package setup-js
  :demand t
  :load-path "./bob-lisp")

(use-package org-setup
  :if (window-system)
  :demand t
  :load-path "./bob-lisp"
  :bind
  ("C-c a" . org-agenda)
  ("C-c c" . org-capture)
  ("C-c S" . org-save-all-org-buffers)
  ("C-c r" . #'my/refresh-google-calendar)
  ("C-c v" . org-brain-visualize)
  ("C-c l" . org-store-link))

(use-package restclient
  :init
  (add-to-list 'auto-mode-alist '("\\.client$" . restclient-mode))
  :if (window-system))
(use-package avy
  :disabled t
  :demand
  :bind
  ("C-c M-c" . avy-goto-char)
  ("C-c M-d" . avy-goto-word-1))
(use-package ace-jump-mode
  :init
  (setq ace-jump-mode-case-fold nil)
  :demand
  :bind
  ("C-c M-c" . ace-jump-mode))
(use-package ripgrep
  :bind ("C-c M-r" . ripgrep-regexp))

(use-package setup-snippets
  :demand t
  :bind ("C-c TAB" . yas-expand)
  :load-path "./bob-lisp")

(use-package expand-region
  :bind ("M-#" . er/expand-region))
(use-package anzu
  :demand t
  :bind (("C-M-%" . anzu-query-replace-regexp)
         ("M-%" . query-replace)
         ("C-c M-%" . anzu-repl))
  :config
  (global-anzu-mode))

(use-package multiple-cursors
  :if (window-system)
  :bind
  (("C->" . 'mc/mark-next-like-this)
   ("C-<" . 'mc/mark-previous-like-this)))

(use-package setup-rust
  :after (lsp-setup)
  :demand t
  :bind (:map rust-mode-map
              ("C-c C-c C-r" . rust-run)
              ("C-c C-c C-b" . rust-compile)
              ("C-c C-c C-s". sbt-switch-to-active-sbt-buffer))
  :load-path "./bob-lisp")

(use-package kubernetes
  :commands (kubernetes-overview)
  :init
  :hook (kubernetes-overview-mode . (lambda () (setenv "AWS_PROFILE" "dev-k8s")))
  :config
  (setq kubernetes-poll-frequency 30)
  (setq kubernetes-redraw-frequency 30)
  :bind
  ("C-c k" . kubernetes-overview))

(use-package kubernetes-tramp)

(use-package zoom-window
  :init
  (custom-set-variables
   '(zoom-window-mode-line-color "DarkBlue"))
  :bind ("C-x C-z" . zoom-window-zoom))

(use-package evil) ;; Sometimes you might turn into the dark side
(use-package haskell-mode)
(put 'set-goal-column 'disabled nil)

(use-package elm-mode)
(put 'dired-find-alternate-file 'disabled nil)

(use-package treemacs
  :disabled t
  :demand t)

(use-package treemacs-projectile
  :disabled t
  :after treemacs)

(use-package control-mode
  :init (control-mode-default-setup))

(use-package elpy
  :init
  (advice-add 'python-mode :before 'elpy-enable))

(use-package try)

;; This is for term mode act nice with other frame
(define-key term-raw-map (kbd "M-o") 'other-frame)
(put 'narrow-to-region 'disabled nil)
