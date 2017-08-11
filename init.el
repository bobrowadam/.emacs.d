;;; package --- Summary ---  My first init.el file
;; Author: Bob Row
;;; Commentary:

;;; Code:
(package-initialize)

;; add elpa and melpa repos
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
	("melpa" . "http://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")))

(unless (package-installed-p 'use-package)
  (progn 
    (package-refresh-contents
     (package-install 'use-package))))

(unless (package-installed-p 'req-package)
  (progn 
    (package-refresh-contents
     (package-install 'req-package))))

;;emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)


(use-package s)
(use-package dash)

;; Set shell path 
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

;; Set path to dependencies
(setq settings-dir
      (expand-file-name "settings" user-emacs-directory))
(setq site-lisp-dir
      (expand-file-name "site-lisp" user-emacs-directory))

;; Set up load path
(add-to-list 'load-path settings-dir)
(add-to-list 'load-path site-lisp-dir)

(setq initial-scratch-message "Greetings master Bob, welcome back.\nWhat shell we do today sir?")

;; Setup Start Dashboard:
(use-package dashboard
  :ensure t
  :init
  (setq dashboard-banner-logo-title "Welcome to Emacs Dashboard")
  ;; (setq dashboard-startup-banner "~/Pictures/never-go-full-retard-tee_design_small.png")
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-items '((recents  . 5)
                          (agenda . 5)
                          (projects . 5)))
  :config
  (dashboard-setup-startup-hook))

(use-package setup-smartparens)
(use-package setup-org)
(use-package setup-yas)

;; Functions (load all files in defuns-dir)
(setq defuns-dir (expand-file-name "defuns" user-emacs-directory))
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))

;; Lets start with a attering of sanity
(use-package sane-defaults)

;; appearance
(use-package appearance)

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; Save point position between sessions
(use-package saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

;; this is usefull for joining lines with M-^ so you could also do M--^
(global-set-key (kbd "M-_") 'negative-argument)

;; C-w kill line on point
(use-package whole-line-or-region
  :ensure t
  :config
  (whole-line-or-region-mode 1))

;; imenu lets you jump around for jumping to points of interest in a buffer
(global-set-key (kbd "M-i") 'imenu)

;; flycheck
(use-package setup-flycheck)
(use-package setup-js2-mode)
(use-package setup-common-lisp)
(use-package setup-paredit)

(use-package projectile
  :ensure t
  :config
  (projectile-mode))

;; Install  Ag.el in order for projectile ag to work:
(use-package ag
  :ensure t)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Shows available keys
(use-package which-key
  :ensure t
  :config
  (which-key-mode +1))

;; Load setup files:
(use-package setup-ido)
(use-package setup-dired)
(use-package setup-magit)
(use-package setup-eshell)
(eval-after-load 'grep '(use-package setup-rgrep))
(eval-after-load 'prodigy '(use-package setup-pipeline))
(eval-after-load 'tramp '(use-package setup-tramp))

(load "key-bindings")

;; Visual regexp
(use-package visual-regexp
  :ensure t
  :config
  (define-key global-map (kbd "M-%") 'vr/query-replace)
  (define-key global-map (kbd "C-M-%") 'vr/replace))

;; pdf - tools:
(use-package pdf-tools
  :ensure t
  :config (pdf-tools-install))

(use-package expand-region
  :ensure t
  :bind ("C-M-S-SPC" . er/expand-region))

(use-package multiple-cursors :ensure t)

;; Search
(use-package flex-isearch
  :ensure t
  :bind
  (("C-c s" . flex-isearch-forward)
   ("C-c r" . flex-isearch-backward))
  :config (global-flex-isearch-mode))

(global-set-key (kbd "C-s") 'isearch-forward-use-region)
(global-set-key (kbd "C-r") 'isearch-forward-use-region)

;; flyspell
(use-package flyspell-correct-helm
  :ensure t
  :config
  (define-key flyspell-mode-map (kbd "C-;") 'flyspell-correct-previous-word-generic)
  (global-set-key (kbd "C-;") 'flyspell-correct-word-generic))


;; worksapces setup:
(use-package setup-perspective)

;; windows with ace-window
(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "C-x o") 'ace-window )
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq aw-dispatch-always t))

(use-package pretty-mode
  :ensure t
  :config (add-hook 'js2-mode-hook 'turn-on-pretty-mode))

(use-package mode-icons
  :ensure t
  :config (mode-icons-mode))

(use-package slime
  :ensure t
  :config (setq inferior-lisp-program "/usr/local/bin/clisp"))

(use-package ibuffer-vc
  :ensure t
  :init
  (global-set-key (kbd "C-x C-b") 'ibuffer)
  :config
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-vc-set-filter-groups-by-vc-root)
              (unless (eq ibuffer-sorting-mode 'recency)
                (ibuffer-do-sort-by-recency)))))



;;Speed typing tutor:
(use-package speed-type
  :ensure t)

;; increment numbers:
(use-package shift-number
  :ensure t
  :config
  (global-set-key (kbd "M-C-+") 'shift-number-up)
  (global-set-key (kbd "M-C-_") 'shift-number-down))


(provide 'init)

;;; init.el ends here
(put 'narrow-to-region 'disabled nil)
