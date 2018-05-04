;;; Begin initialization
;; Turn off mouse interface early in startup to avoid momentary display

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(when window-system
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1)
  (blink-cursor-mode -1)
  (display-time-mode 1))
(setq use-package-debug t)
(setq inhibit-startup-message t)
(setq ring-bell-function 'ignore
      visible-bell nil)

;; Lines should be 80 characters wide, not 72
(setq fill-column 80)

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

(setq locale-coding-system 'utf-8) ; pretty
(set-terminal-coding-system 'utf-8) ; pretty
(set-keyboard-coding-system 'utf-8) ; pretty
(set-selection-coding-system 'utf-8) ; please
(prefer-coding-system 'utf-8) ; with sugar on top

;; (load-theme 'tango-dark)
;; (set-default-font "hack 14")

(use-package misc-funcs)
(use-package remote-defuns)
(global-set-key (kbd "C-x j") 'whitespace-cleanup)

;; custom file path
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(unless (not (file-exists-p custom-file))
  (load custom-file))

;; delete char and delte word with "C-h" "C-M-h"
(define-key key-translation-map [?\C-h] [?\C-?])
(define-key key-translation-map (kbd "<f1>") (kbd "C-h"))
(global-set-key (kbd "M-C-h") 'backward-kill-word)

(require 'server)
(unless (server-running-p)
  (server-start))

(use-package linum-off
  :ensure
  :config
  (global-linum-mode 1))

(use-package paredit
  :ensure t
  :hook
  ((emacs-lisp-mode cider-repl-mode cider-mode) . paredit-mode))

(use-package smartparens
  :ensure t
  :hook ((js2-mode . smartparens-mode)
         (nodejs-repl-mode . smartparens-mode)
         (fundamental-mode . smartparens-mode)
         (text-mode . smartparens-mode))
  :config
  (show-smartparens-global-mode t))

(use-package company
  :ensure t
  :config
  (global-company-mode t))

(use-package arjen-grey-theme
  :ensure t
  :config
  (set-default-font "Latin Modern Mono 16")
  (load-theme 'tango-dark))

(use-package smex
  :ensure t)

(use-package ivy
  :ensure t
  :config
  (use-package flx
    :ensure t)
  (setq ivy-re-builders-alist
        '((t . ivy--regex-fuzzy)))
  (setq ivy-initial-inputs-alist nil)
  (ivy-mode 1))

(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)))

(use-package whole-line-or-region
  :ensure t
  :config
  (whole-line-or-region-global-mode t))

(use-package js2-mode
  :ensure t
  :init
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
  (defun unset-electric-indent ()
    (electric-indent-mode nil))
  :hook ((js2-mode . js2-imenu-extras-mode)
         (js2-mode . js2-mode-hide-warnings-and-errors)
         (js2-mode . electric-indent-mode))
  :bind (:map js2-mode-map ("C-<tab>" . js2-indent-bounce))
  :config
  (setq-default js2-auto-indent-p nil)
  (setq-default unset-electric-indent)
  (setq-default js-indent-level 2))

(use-package highlight-indent-guides
  :ensure t
  :hook (js2-mode . highlight-indent-guides-mode))

(use-package tern
  :ensure t
  :hook (js2-mode . tern-mode))

(use-package company-tern
  :after tern
  :ensure t
  :config
  (add-to-list 'company-backends 'company-tern))

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :config
  (setq magit-completing-read-function 'ivy-completing-read)
  (magit-define-popup-switch 'magit-push-popup
    ?t "Follow tags" "--follow-tags"))

(use-package magithub
  :ensure t
  :after magit
  :config
  (magithub-feature-autoinject t)
  (setq epa-pinentry-mode 'loopback))

(use-package anzu
  :ensure t
  :config
  (global-anzu-mode +1))

(global-set-key (kbd "C-c C-p") 'open-line-above)
(global-set-key (kbd "C-c C-n") 'open-line-below)
(global-set-key (kbd "C-c C-k") 'kill-and-retry-line)
(global-set-key (kbd "C-}") 'enlarge-window-horizontally)
(global-set-key (kbd "C-{") 'shrink-window-horizontally)

(use-package zoom-window
  :ensure t
  :init
  (custom-set-variables
   '(zoom-window-mode-line-color "DarkGreen"))
  :bind ("C-x C-z" . zoom-window-zoom))

(use-package dired
  :config
  (setq dired-listing-switches "-alh"))

(use-package tramp
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
  (setq tramp-default-method "ssh")
  ;;Disable projectile mode line project naming for better performance:
    ;; (add-hook 'find-file-hook
    ;;           (lambda ()
    ;;             (when (file-remote-p default-directory)
    ;;               (setq-local projectile-mode-line "Projectile"))))
  )

(use-package flycheck
  :ensure t
  :hook ((js2-mode . flycheck-mode)
         (flycheck-mode . my/use-eslint-from-node-modules)))

(use-package cider
  :ensure t)

(use-package which-key
  :ensure t
  :config
  (which-key-mode 1))

(use-package projectile
  :ensure t
  :init
  (defadvice projectile-on (around exlude-tramp activate)
    "This should disable projectile when visiting a remote file"
    (unless  (--any? (and it (file-remote-p it))
                     (list
                      (buffer-file-name)
                      list-buffers-directory
                      default-directory
                      dired-directory))
      ad-do-it))
  :ensure t
  :config
  (setq projectile-mode-line "Projectile")
  (setq projectile-completion-system 'ivy)
  (setq projectile-switch-project-action #'projectile-dired)
  (projectile-mode t))


(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package nodejs-repl
  :ensure t)
