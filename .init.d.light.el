;;; Begin initialization
;; Turn off mouse interface early in startup to avoid momentary display

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)
(setq display-time-day-and-date t)
(setq display-time-default-load-average nil)

(when window-system
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1)
  (blink-cursor-mode -1)
  (display-time))

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

(setq custom-safe-themes t)
(if (memq window-system '(mac ns))
      (set-default-font "Sauce Code Powerline 16")
    (set-default-font "Latin Modern Mono 16"))
  (load-theme 'manoj-dark)

(use-package smart-mode-line
  :ensure t
  :config
  (sml/setup))

(use-package misc-funcs)
(use-package remote-defuns)
(global-set-key (kbd "C-x j") 'whitespace-cleanup)
(global-set-key (kbd "M-i") 'imenu)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; custom file path
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(unless (not (file-exists-p custom-file))
  (load custom-file))

;; delete char and delte word with "C-h" "C-M-h"
(define-key key-translation-map [?\C-h] [?\C-?])
(define-key key-translation-map (kbd "<f1>") (kbd "C-h"))
(global-set-key (kbd "M-C-h") 'backward-kill-word)

(defun unset-electric-indent ()
    (electric-indent-mode -1))

(use-package linum-off
  :ensure
  :config
  (global-linum-mode 1))

(use-package paredit
  :init
  (defun use-paredit-not-sp ()
    "Use paredit and stop using Smartparens."
    (paredit-mode 1)
    (turn-off-smartparens-mode))
  :ensure t
  :hook
  ((emacs-lisp-mode cider-repl-mode cider-mode) . use-paredit-not-sp))

(use-package smartparens
  :ensure t
  :bind ("C-)" . sp-unwrap-sexp)
  :config
  (smartparens-global-mode)
  (show-smartparens-global-mode t))

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
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)))

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
  :hook ((js2-mode . js2-imenu-extras-mode)
         (js2-mode . js2-mode-hide-warnings-and-errors)
         (js2-mode . electric-indent-mode)
         (js2-mode . yas-minor-mode))
  :bind (:map js2-mode-map ("C-<tab>" . js2-indent-bounce))
  :config
  (setq-default js2-auto-indent-p nil)
  (setq-default unset-electric-indent)
  (setq-default js-indent-level 2))

(use-package highlight-indent-guides
  :ensure t
  :hook (js2-mode . highlight-indent-guides-mode))

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
  (setq dired-listing-switches "-alh")
  (use-package dired-x
    :hook (dired-mode . dired-omit-mode)))

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
  (eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))
  (setq vc-ignore-dir-regexp
                  (format "\\(%s\\)\\|\\(%s\\)"
                          vc-ignore-dir-regexp
                          tramp-file-name-regexp))
  
  ;; (setq remote-file-name-inhibit-cache nil)
  ;; (setq tramp-use-ssh-controlmaster-options t)
  ;; (setq tramp-ssh-controlmaster-options
  ;;       (concat
  ;;        "-o ControlPath=/tmp/ssh-ControlPath-%%r@%%h:%%p "
  ;;        "-o ControlMaster=auto -o ControlPersist=no"))
  )

(use-package inf-mongo
  :ensure t)
