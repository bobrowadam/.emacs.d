(setq gc-cons-threshold 100000000)
(setq debug-on-error nil)
(setq package-enable-at-startup nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq package-native-compile nil)
(setq use-package-always-defer t)
(setq use-package-always-ensure t)
(setq use-package-verbose t)
(setq use-package-debug t)
(setq use-package-compute-statistics t)

(use-package startup
  :demand t
  :load-path "./modules")

(use-package basic-settings
  :after startup
  :load-path "./modules"
  :demand t)

(use-package appearance
  :after startup
  :load-path "./modules"
  :demand t)

(use-package navigation
  :after startup
  :demand t
  :load-path "./modules")

(use-package files-and-buffers
  :after startup
  :demand t
  :load-path "./modules")

(use-package prog
  :after startup
  :demand t
  :load-path "./modules")

(use-package setup-magit
  :after startup
  :demand t
  :load-path "./modules")

(use-package setup-org
  :after startup
  :demand t
  :load-path "./modules")

(use-package inf-mongo
  :after startup
  :after org)

(use-package shell-defuns
  :after startup
  :if (window-system)
  :load-path "./site-lisp"
  :demand t)

(use-package utils
  :after startup
  :load-path "./site-lisp"
  :ensure nil)

(use-package edit-funcs
  :after startup
  :if (window-system)
  :demand t
  :load-path "./site-lisp"
  :bind
  ("C-`" . unpop-to-mark-command)
  ("M-`" . jump-to-mark))

(use-package vterm
  :after startup
  :if (window-system)
  :after shell-defuns
  :config
  (setq vterm-max-scrollback 100000)
  (define-key vterm-mode-map [remap whole-line-or-region-yank] #'vterm-yank)
  :bind
  ("C-c s s". bob/projectile-run-vterm)
  ("C-c s e" . bob/vterm)
  ("C-c s j" . bob/jump-to-shell)
  (:map vterm-mode-map ("C-c C-j" . vterm-copy-mode))
  (:map vterm-copy-mode-map ("C-c C-j" . vterm-copy-mode)))

(use-package tramp
  :ensure nil
  :init (setq tramp-verbose 6)
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
  (setq remote-file-name-inhibit-cache 3600
        tramp-completion-reread-directory-timeout nil
        vc-ignore-dir-regexp (format "%s\\|%s"
                                     vc-ignore-dir-regexp
                                     tramp-file-name-regexp))
  (setq tramp-histfile-override t)
  ;; Save backup files locally
  ;; from https://stackoverflow.com/a/47021266
  (add-to-list 'backup-directory-alist
               (cons tramp-file-name-regexp "/tmp/emacs-backup/")))

(use-package scratch-pop
  :bind ("C-c r" . scratch-pop))

(use-package avy
  :init (setq avy-case-fold-search nil)
  :bind
  ("C-c M-d" . avy-goto-char-in-line)
  ("C-c M-c" . avy-goto-word-1))

(use-package highlight-indent-guides
  :init
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-responsive 'top)
  :hook (prog-mode . highlight-indent-guides-mode))

(use-package json-mode)
(use-package jq-format
  :after json-mode)

(use-package origami
  :bind (:map origami-mode-map
              ("C-=" . origami-toggle-node)))

(use-package csv-mode)
(use-package ace-window
  :bind ( "C-x o" . ace-window)
  :config
  (setq aw-scope 'frame)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package dockerfile-mode)
(use-package shell-command+
  :bind ("M-!" . shell-command+))

(use-package haskell-mode :disabled t)
(use-package haskell-snippets :disabled t)
(use-package cider :disabled t)
(use-package clojure-mode :disabled t)
(use-package sicp)

(use-package elfeed
  :init
  (setq elfeed-feeds
        '(
          ;; ("https://www.reddit.com/r/listentothis/.rss" music reddit)
          ("https://www.reddit.com/r/emacs/.rss" programming emacs reddit)
          ("http://notarbut.co/feed/podcast" podcast)
          ("https://blog.rust-lang.org/feed.xml" programming rust)
          ;; ("https://www.reddit.com/r/rust/.rss" programming rust reddit)
          ;; ("https://www.reddit.com/r/Clojure/.rss" programming clojure reddit)
          ("https://danluu.com/atom.xml" programming blog)
          ("https://feed.podbean.com/geekonomy/feed.xml" podcast)
          ("https://protesilaos.com/master.xml" programming blog)
          ))
  :bind ("C-c w" . elfeed))

(use-package slime
  :disabled t
  :config
  (setq inferior-lisp-program "/usr/local/bin/sbcl")
  (slime-setup '(slime-fancy slime-company)))

(use-package slime-company
  :disabled t
  :after slime)

(use-package sly
  :init
  (setq inferior-lisp-program "/usr/local/bin/sbcl")
  :hook
  (sly-mode . (lambda ()
     (unless (sly-connected-p)
       (save-excursion (sly))))))

(use-package sly-asdf
  :disabled t)
(use-package sly-quicklisp
  :disabled t)

(use-package racket-mode
  :disabled t)

(use-package docker)
(use-package docker-tramp)

(use-package dirvish
  :disabled t
  :init
  (dirvish-override-dired-mode))

(use-package pdf-tools)
(use-package hcl-mode
  :mode
  ("\\.dsl\\'" . hcl-mode))

(use-package iedit
  :bind ("C-;" . iedit-mode))

(use-package grammarly)
(use-package flycheck-grammarly)
(use-package prettier)
(use-package pandoc-mode)

(put 'dired-find-alternate-file 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'magit-clean 'disabled nil)
