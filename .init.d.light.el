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
(set-default-font "Latin Modern Mono 16")
(load-theme 'manoj-dark)

(use-package misc-funcs)
;; (use-package remote-defuns)
(global-set-key (kbd "C-x j") 'whitespace-cleanup)
(global-set-key (kbd "M-i") 'imenu)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x ;") 'comment-line)


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

(use-package whole-line-or-region
  :ensure t
  :config
  (whole-line-or-region-global-mode t))

;; (use-package dired
;;   :config
;;   (setq dired-listing-switches "-alh"))

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
