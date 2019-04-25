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
  :init
  (setq rm-whitelist (setq rm-whitelist (mapconcat #'identity '( " Paredit" " Smartparens") "\\|")))
  (rich-minority-mode 1))
(use-package flycheck
  :demand t)
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
  :demand t
  :load-path "./bob-lisp")
(use-package setup-magit
  :demand t
  :load-path "./bob-lisp")
(use-package whole-line-or-region
  :demand t
  :init (whole-line-or-region-mode 1))
(use-package company
  :demand t
  :config (global-company-mode 1))
(use-package setup-projectile
  :load-path "./bob-lisp"
  :demand t)
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
  :demand t
  :load-path "./bob-lisp")
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
(use-package yasnippet
  :demand
  :hook
  (prog-mode-hook . yas-minor-mode)
  :config
  (setq yas-snippet-dirs
      `(,(concat user-emacs-directory "snippets")))
  :config (yas-reload-all))
(use-package yasnippet-snippets)
(use-package ammonite-mode
  :demand t
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
  :demand t
  :load-path "./bob-lisp")
(use-package kubernetes
  :commands (kubernetes-overview))
