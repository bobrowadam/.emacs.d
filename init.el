;;; Begin initialization

(package-initialize)
(setq display-time-day-and-date t)
(setq display-time-default-load-average nil)
(when (window-system)
  (setq confirm-kill-emacs 'yes-or-no-p))

(when window-system
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1)
  (blink-cursor-mode -1)
  (display-time))

(global-subword-mode t)
(global-superword-mode -1)
(setq use-package-debug t)
(setq inhibit-startup-message t)
(setq ring-bell-function 'ignore
      visible-bell nil)

;; Lets you use minibuffer while in minibuffer:
(setq enable-recursive-minibuffers t)

;; Lines should be 80 characters wide, not 72
(setq fill-column 80)

;; Push mark before exprestion jumping
;; (advice-add 'forward-sexp :before (lambda (&rest args) (push-mark)))
;; (advice-add 'backward-sexp :before (lambda (&rest args) (push-mark)))

(delete-selection-mode 1)
(set-default 'indent-tabs-mode nil)
(global-auto-revert-mode 1)
(auto-compression-mode t)
(defalias 'yes-or-no-p 'y-or-n-p)

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")))

(add-to-list 'load-path "~/.emacs.d/my-funcs")

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

;; Theme and font
(use-package gruber-darker-theme
  :if (window-system)
  :ensure t
  :init (setq custom-safe-themes t)
  :config
  (set-default-font "Latin Modern Mono 18")
  (add-to-list 'default-frame-alist
               '(font . "Latin Modern Mono 18"))
  (load-theme 'gruber-darker))

(use-package smart-mode-line
  :if (window-system)
  :ensure t
  :config
  (sml/setup))

(use-package misc-funcs)
(use-package remote-defuns)
(use-package edit-funcs)

(global-set-key (kbd "C-x j") 'whitespace-cleanup)
(global-set-key (kbd "M-i") 'imenu)

;; custom file path
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(unless (not (file-exists-p custom-file))
  (load custom-file))

;; delete char and delte word with "C-h" "C-M-h"
(define-key key-translation-map [?\C-h] [?\C-?])
(define-key key-translation-map (kbd "<f1>") (kbd "C-h"))
(global-set-key (kbd "M-C-h") 'backward-kill-word)

(if window-system
 (progn (require 'server)
  (unless (server-running-p)
    (server-start))))

(defun unset-electric-indent ()
    (electric-indent-mode -1))

(use-package linum-off
  :if (memq window-system '(mac ns))
  :ensure
  :config
  (global-linum-mode 1))

(use-package paredit
  :if (window-system)
  :init
  (defun use-paredit-not-sp ()
    "Use paredit and stop using Smartparens."
    (paredit-mode 1)
    (turn-off-smartparens-mode))
  :ensure t
  :hook
  ((emacs-lisp-mode cider-repl-mode cider-mode) . use-paredit-not-sp))

(use-package smartparens
  :if (window-system)
  :ensure t
  :bind ("C-)" . sp-unwrap-sexp)
  :config
  (smartparens-global-mode)
  (show-smartparens-global-mode t))

(use-package company
  :if (window-system)
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
      "rg -i -M 120 --no-heading --line-number --color never %s .")
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-x C-m" . counsel-mark-ring)
         ("C-c C-s r g" . counsel-rg)))
(use-package whole-line-or-region
  :ensure t
  :config
  (whole-line-or-region-global-mode t))

(use-package js2-mode
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
         (js2-mode . my-load-js2-snippets))
  :bind (:map js2-mode-map ("C-<tab>" . js2-indent-bounce))
  :config
  (setq-default js2-auto-indent-p nil)
  (setq-default unset-electric-indent)
  (setq-default js-indent-level 2))

(use-package highlight-indent-guides
  :ensure t
  :hook (js2-mode . highlight-indent-guides-mode))

(use-package tern
  :if (window-system)
  :ensure t
  :hook (js2-mode . tern-mode))

(use-package company-tern
  :if (window-system)
  :after tern
  :ensure t
  :config
  (add-to-list 'company-backends 'company-tern))

(use-package magit
  :if (window-system)
  :ensure t
  :bind ("C-x g" . magit-status)
  :config
  (setq magit-completing-read-function 'ivy-completing-read)
  (magit-define-popup-switch 'magit-push-popup
    ?t "Follow tags" "--follow-tags"))

(use-package magithub
  :if (window-system)
  :ensure t
  :after (magit lastpass)
  :pin melpa
  :config
  (setq epa-pinentry-mode 'loopback)
  (setq magithub-debug-mode t)
  (setq auth-sources '("~/.authinfo" "~/.authinfo.gpg" "~/.netrc"))
  (magithub-feature-autoinject t))

(use-package anzu
  :if (window-system)
  :ensure t
  :config
  (global-anzu-mode +1))

(global-set-key (kbd "C-c C-k") 'kill-and-retry-line)

(use-package zoom-window
  :if (window-system)
  :ensure t
  :init
  (custom-set-variables
   '(zoom-window-mode-line-color "DarkGreen"))
  :bind ("C-x C-z" . zoom-window-zoom))

(use-package dired
  :if (window-system)
  :config
  (setq dired-listing-switches "-alh")
  (use-package dired-x
    :hook (dired-mode . dired-omit-mode)))

(use-package tramp
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
  :after tramp
  :ensure t
  :config
  (require 'docker-tramp-compat))

(use-package flycheck
  :if (window-system)
  :ensure t
  :hook ((js2-mode . flycheck-mode)
         (flycheck-mode . my/use-eslint-from-node-modules)))

(use-package cider
  :if (window-system)
  :ensure t)

(use-package which-key
  :if (window-system)
  :ensure t
  :config
  (which-key-mode 1))

(use-package projectile
  :if (window-system)
  :ensure t
  :init
  (defun my-run-eshell (&optional arg)
    "Create an interactive Eshell buffer.
if in project use `projectile-run-eshell"
    (interactive "P")
    (if (projectile-project-p)
        (projectile-run-eshell)
      (eshell arg)))
  :config
  (setq projectile-completion-system 'ivy)
  (setq projectile-switch-project-action #'projectile-dired)
  (projectile-mode t)
  (bind-key "C-c e" 'my-run-eshell))

(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns))
  ;; :init (setq exec-path-from-shell-arguments '("-l"))
  :config
  (exec-path-from-shell-initialize))

(use-package nodejs-repl
  :if (memq window-system '(mac ns))
  :ensure t)

(use-package ensime
  :if (window-system)
  :pin melpa
  :hook (scala-mode . unset-electric-indent)
  :ensure t)

(use-package sbt-mode
  :if (window-system)
  :pin melpa)

(use-package scala-mode
  :pin melpa
  :config
  (add-to-list 'auto-mode-alist '("\\.sc$" . scala-mode)))

(use-package org-mode
  :if (window-system)
  :bind ("C-c a" . org-agenda))

(use-package org-projectile
  :if (window-system)
  :after (projectile org-mode)
  :ensure t)

(use-package org-bullets
  :if (window-system)
  :ensure t
  :config (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package yasnippet
  :if (window-system)
  :ensure t
  :diminish yas-minor-mode
  :commands yas-minor-mode
  :bind ("C-c TAB" . yas-expand)
  :config (yas-global-mode))

(use-package restclient
  :if (window-system)
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.client$" . restclient-mode)))

(use-package inf-mongo
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
  :if (window-system)
  :ensure t
  :config
  (setq lastpass-user "adam@bigpanda.io")
  (setq lastpass-multifactor-use-passcode t)
  (lastpass-auth-source-enable))

(use-package goto-chg
  :ensure t
  :commands goto-last-change
  ;; complementary to
  ;; C-x r m / C-x r l
  ;; and C-<space> C-<space> / C-u C-<space>
  :bind (("C-." . goto-last-change)
         ("C-," . goto-last-change-reverse)))

(use-package ag
  :if (window-system)
  :ensure t)
(use-package ripgrep
  :if (window-system)
  :ensure t)
(use-package wgrep-ag
  :if (window-system)
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package eshell
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
  :ensure t
  :bind ("M-#" . er/expand-region))

(use-package ibuffer-projectile
  :ensure t
  :hook (ibuffer . ibuffer-projectile-set-filter-groups)
  :bind ("C-x C-b" . ibuffer))

(use-package flyspell-correct
  :ensure t)

(use-package flyspell-correct-ivy
  :ensure t
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-previous-word-generic)))

(use-package dante
  :ensure t
  :after haskell-mode
  :commands dante-mode
  :init
  (add-hook 'haskell-mode-hook 'dante-mode)
  (add-hook 'haskell-mode-hook 'flycheck-mode))
