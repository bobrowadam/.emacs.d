(setq gc-cons-threshold (* 50 1000 1000))

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)
            (setq gc-cons-threshold (* 2 1000 1000))))

(require 'package)
(setq debug-on-error nil)
(setq package-enable-at-startup nil)

(unless package--initialized (package-initialize))

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

(eval-when-compile
  (require 'use-package))
(savehist-mode)

(use-package use-package-chords
  :ensure t
  :config (key-chord-mode 1))

(setq use-package-always-defer t)
(setq use-package-always-ensure t)
(setq use-package-verbose t)
(setq use-package-debug t)
(setq use-package-compute-statistics t)

(use-package dash)
(use-package dap-mode :disabled t)
(use-package setup-dired
  :load-path "./bob-lisp"
  :demand t
  :hook (dired-mode . auto-revert-mode))

(use-package saveplace
  :config
  (setq save-place-file "~/.emacs.d/saveplace")
  (setq save-place-forget-unreadable-files t)
  (save-place-mode 1))

(use-package f)
(use-package server
  :init (server-start))

(use-package which-key
  :demand t
  :if (window-system)
  :config
  (which-key-mode 1))

(use-package exec-path-from-shell
  :if (window-system)
  :demand t
  :init (setq exec-path-from-shell-arguments '("-l"))
  :config (exec-path-from-shell-initialize))

(use-package flycheck)

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
  ("C-x k" . kill-this-buffer)
  ("M-SPC" . cycle-spacing)
  :chords
  (("jj" . "_")
   ("ii" . "|")
   ("qq" . "~")))

(use-package setup-eshell
  :disabled
  :if (window-system)
  :demand t
  :load-path "./bob-list")

(use-package tramp-settings
  :demand t
  :if (window-system)
  :load-path "./bob-lisp")

(use-package remote-defuns
  :load-path "./bob-lisp")

(use-package misc-funcs
  :if (window-system)
  :demand t
  :load-path "./bob-listp")

(use-package edit-funcs
  :if (window-system)
  :demand t
  :load-path "./bob-lisp"
  :bind
  ("C-`" . unpop-to-mark-command)
  ("M-`" . jump-to-mark))

(use-package setup-parens
  :if (window-system)
  :load-path "./bob-listp"
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
              ("M-W" . sp-copy-sexp)))

(use-package inf-mongo)

(use-package ibuffer-setup
  :demand
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
  :if (window-system)
  :bind
  ("C-x g" . magit-status)
  :demand t
  :load-path "./bob-lisp")

(use-package whole-line-or-region
  :demand t
  :init (whole-line-or-region-mode 1))

(use-package company
  :if (window-system)
  :demand t
  :init
  (setq company-tooltip-align-annotations t)
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0.2)
  :config (global-company-mode 1))

(use-package setup-projectile
  :if (window-system)
  :load-path "./bob-lisp"
  :demand t
  :bind
  ("C-c e" . my-run-eshell))

(use-package sicp)

(use-package ace-window
  :bind ( "C-x o" . ace-window)
  :config
  (setq aw-scope 'frame)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package highlight-indent-guides :if (window-system))

(use-package lsp-setup
  :if (window-system)
  :demand t
  :load-path "./bob-lisp")

(use-package scala-setup
  :disabled
  :if (window-system)
  :after (company lsp-setup)
  :demand t
  :load-path "./bob-lisp")

(use-package setup-js
  :if (window-system)
  :demand t
  :load-path "./bob-lisp")

(use-package haskell-setup
  :disabled
  :if (window-system)
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
  :init (setq avy-case-fold-search nil)
  :bind
  ("C-c M-d" . avy-goto-char-in-line)
  ("C-c M-c" . avy-goto-word-1))

(use-package ace-jump-mode
  :disabled
  :init
  (setq ace-jump-mode-case-fold nil)
  :demand
  :bind
  ("C-c M-c" . ace-jump-mode))

(use-package ripgrep
  :init (setq wgrep-auto-save-buffer t)
  :bind
  ("C-c M-r" . ripgrep-regexp)
  (:map ripgrep-search-mode-map ("C-x C-q" . ivy-wgrep-change-to-wgrep-mode)))

(use-package deadgrep
  :bind ("C-c C-d" . deadgrep))

(use-package wgrep)

(use-package setup-snippets
  :if (window-system)
  :demand t
  :bind ("C-c TAB" . yas-expand)
  :load-path "./bob-lisp")

(use-package expand-region
  :bind ("M-#" . er/expand-region))

(use-package anzu
  :if (window-system)
  :demand t
  :bind (("C-M-%" . anzu-query-replace-regexp)
         ("M-%" . anzu-query-replace))
  :config
  (global-anzu-mode 1))

(use-package multiple-cursors
  :if (window-system)
  :bind
  (("C->" . 'mc/mark-next-like-this)
   ("C-<" . 'mc/mark-previous-like-this)))

(use-package setup-rust
  :if (window-system)
  :after (lsp-setup)
  :demand t
  :bind (:map rust-mode-map
              ("C-c C-c C-r" . rust-run)
              ("C-c C-c C-b" . rust-compile)
              ("C-c C-c C-s". sbt-switch-to-active-sbt-buffer))
  :load-path "./bob-lisp")

(use-package kubernetes
  :disabled
  :commands (kubernetes-overview)
  :init
  :hook (kubernetes-overview-mode . (lambda () (setenv "AWS_PROFILE" "dev-k8s")))
  :config
  (setq kubernetes-poll-frequency 30)
  (setq kubernetes-redraw-frequency 30)
  :bind
  ("C-c k" . kubernetes-overview))

(use-package zoom-window
  :init
  (custom-set-variables
   '(zoom-window-mode-line-color "DarkBlue"))
  :bind ("C-x C-z" . zoom-window-zoom))

(use-package control-mode
  :if (window-system)
  :init (control-mode-default-setup))

(use-package elpy
  :disabled
  :if (window-system)
  :init
  (advice-add 'python-mode :before 'elpy-enable))

(use-package try)

(use-package shell-defuns :load-path "./bob-lisp" :demand t)
(use-package vterm
  :if (window-system)
  :after shell-defuns
  :demand t
  :config
  (define-key vterm-mode-map [remap whole-line-or-region-yank] #'vterm-yank)
  :bind
  ("C-c s s". bob/projectile-run-vterm)
  ("C-c s e" . bob/vterm)
  ("C-c s j" . bob/jump-to-shell)
  (:map vterm-mode-map ("C-c C-j" . vterm-copy-mode))
  (:map vterm-copy-mode-map ("C-c C-j" . vterm-copy-mode)))

(use-package setup-web-dev
  :demand t
  :load-path "./bob-lisp")

(use-package perspective
  :disabled
  :demand t
  :bind
  ("C-x b" . persp-ivy-switch-buffer)
  ("C-x C-b" . persp-ibuffer)
  :hook (kill-emacs . persp-state-save)
  :config
  (setq persp-state-default-file (concat user-emacs-directory ".persp-state"))
  (persp-state-load persp-state-default-file))

(use-package yaml-mode)

(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'set-goal-column 'disabled nil)

;;; init.el ends here
