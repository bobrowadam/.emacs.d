;;; package --- Summary ---  My first init.el file
;; Author: Bob Row
;;; Commentary:

;;; Code:
(package-initialize)

;;emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

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
                          (projects . 5)
                          ))
  :config
  (dashboard-setup-startup-hook))

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

;; Set shell path 
(when (memq window-system '(mac ns))
  (use-package exec-path-from-shell
    :ensure t
    :config
    (exec-path-from-shell-initialize)))

;; Set path to dependencies
(setq settings-dir
      (expand-file-name "settings" user-emacs-directory))
(setq site-lisp-dir
      (expand-file-name "site-lisp" user-emacs-directory))

;; Set up load path
(add-to-list 'load-path settings-dir)
(add-to-list 'load-path site-lisp-dir)

;; Smartparens
(load "setup-smartparens")
(load "setup-org")

;; Functions (load all files in defuns-dir)
(setq defuns-dir (expand-file-name "defuns" user-emacs-directory))
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))

;; Lets start with a attering of sanity
(require 'sane-defaults)

;; appearance
(require 'appearance)

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

;; this is usefull for joining lines with M-^ so you could also do M--^
(global-set-key (kbd "M-_") 'negative-argument)

;; C-w kill line on point
(use-package whole-line-or-region
  :ensure t
  :config
  (whole-line-or-region-mode 1))

;; helm
;; (require 'helm-config)

;; imenu lets you jump around for jumping to points of interest in a buffer
(global-set-key (kbd "M-i") 'imenu)

;; flycheck
(load "setup-flycheck")

(add-hook 'js2-mode-hook 'highlight-indent-guides-mode)
(add-hook 'js2-mode-hook 'js2-mode-hide-warnings-and-errors)

;; js2
(load "setup-js2-mode")

;; eslint configuration:
(add-hook 'after-init-hook #'global-flycheck-mode)
(setenv "path" (concat (getenv "path") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

;; magit configuratinon
(global-set-key (kbd "C-x g") 'magit-status)
(use-package magithub
  :ensure t
  :config
  (magithub-feature-autoinject t)
  (setq magithub-clone-default-directory (expand-file-name "source" user-home-directory )))

;; Lisp
(require 'setup-common-lisp)

;; Paredit
(load "setup-paredit")

;; projectile:
(use-package projectile
  :ensure t
  :config
  (projectile-mode))

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Shows available keys
(use-package which-key
  :ensure t
  :config
  (which-key-mode +1))

;; Load setup files:
(eval-after-load 'ido '(require 'setup-ido))
(eval-after-load 'dired '(require 'setup-dired))
(eval-after-load 'magit '(require 'setup-magit))
(eval-after-load 'grep '(require 'setup-rgrep))
(eval-after-load 'eshell '(require 'setup-eshell))
(eval-after-load 'prodigy '(require 'setup-pipeline))
(eval-after-load 'tramp '(require 'setup-tramp))

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

;; (require 'delsel)
;; (require 'wgrep)

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
(require 'setup-perspective)

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
