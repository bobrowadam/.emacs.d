;; Use Package Init ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq package-user-dir "~/source/emacs_configs/trampconfig/elpa")
(setq user-emacs-directory "~/source/emacs_configs/trampconfig/")
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(unless (not (file-exists-p custom-file))
  (load custom-file))

(require 'package)
(package-initialize)
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
(require 'f)

(use-package rich-minority
  :init
  (setq rm-whitelist (setq rm-whitelist (mapconcat #'identity '( " Paredit" " Smartparens") "\\|")))
  (rich-minority-mode 1))

(use-package basic-settings
  :demand t
  :load-path "./bob-lisp")

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

(use-package paredit
  :demand t
  :hook (emacs-lisp-mode . (lambda () (paredit-mode 1))))

(use-package smartparens
  :demand t
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
  (show-smartparens-global-mode t))

(use-package inf-mongo)
(use-package ibuffer-setup :demand t)

(use-package ido-setup
  :demand t
  :load-path "./bob-lisp")

(require 'dired)
(require 'dired-x)

(use-package magit
  :bind ("C-x g" . magit-status))

(use-package whole-line-or-region
  :demand t
  :init (whole-line-or-region-mode 1))

(use-package company
  :demand t
  :config (global-company-mode 1))

(use-package projectile
  :demand t
  :config (projectile-global-mode 1)
  :bind (:map projectile-mode-map ("C-c p" . projectile-command-map)))

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

(use-package scala-setup
  :after company
  :demand t
  :load-path "./bob-lisp")

(use-package setup-js
  :load-path "./bob-lisp"
  :demand t)

(use-package org-setup
  :demand t
  :load-path "./bob-lisp"
  :bind
  ("C-c a" . org-agenda)
  ("C-c c" . org-capture)
  ("C-c S" . org-save-all-org-buffers)
  ("C-c r" . #'my/refresh-google-calendar))

(use-package restclient
  :init
  (add-to-list 'auto-mode-alist '("\\.client$" . restclient-mode))
  :if (window-system))

(use-package avy
  :demand
  :bind
  ("C-c M-c" . avy-goto-char)
  ("C-c M-s" . avy-goto-word-1))

(use-package ripgrep
  :bind ("C-c M-r" . ripgrep-regexp))

(use-package eshell-prompt-extras
  :if (window-system)
  :config
  (autoload 'epe-theme-lambda "eshell-prompt-extras")
  (setq eshell-highlight-prompt nil
        eshell-prompt-function 'epe-theme-lambda))

(use-package yas-snippet)

(use-package ammonite-mode
  :demand t
  :load-path "./bob-lisp")
