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

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Bootstrap `req-package'
(unless (package-installed-p 'req-package)
  (package-refresh-contents)
  (package-install 'req-package))

(use-package benchmark-init
  ;; :disabled t
  :ensure t)

;;emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(use-package s)
(use-package dash)
;; (unless (server-running-p)
  ;; (server-start))


;; Set shell path
(use-package exec-path-from-shell
  :ensure t
  :init
  (setq exec-path-from-shell-debug nil)
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

;; (setq initial-scratch-message "Good day!")

(use-package auto-complete
  :disabled t
  :ensure t
  :config
  (ac-config-default)
  (global-auto-complete-mode t))

(use-package company
  :ensure t
  :config ())

;; Setup Start Dashboard:
(use-package dashboard
  :disabled
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
(use-package setup-clojure)
(use-package misc-defuns
  :config
  (use-package my-misc))

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
(use-package setup-scala)
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
(use-package setup-rgrep)
(use-package prodigy
  :ensure t
  :config
  (use-package setup-pipeline))

(use-package setup-tramp)

;; Visual regexp
(use-package visual-regexp
  :ensure t
  :config
  (define-key global-map (kbd "M-%") 'vr/query-replace)
  (define-key global-map (kbd "C-M-%") 'vr/replace))

;; pdf - tools:
;; (use-package pdf-tools
;;   :ensure t
;;   :config (pdf-tools-install))

(use-package expand-region
  :ensure t
  :bind ("C-M-S-SPC" . er/expand-region))

(use-package multiple-cursors
  :ensure t
  :bind
  ("C->" . mc/mark-next-like-this)
  ("C-c C->" . mc/mark-all-like-this))

;; Search
(use-package flex-isearch
  :disabled t
  :ensure t
  :bind
  (("C-c s" . flex-isearch-forward)
   ("C-c r" . flex-isearch-backward))
  :config (global-flex-isearch-mode))

(use-package counsel
  :ensure t
  :bind
  (("C-c s" . swiper)
   ("C-c M-s a g" . counsel-ag)))

;; (global-set-key (kbd "C-s") 'isearch-forward-use-region)
;; (global-set-key (kbd "C-r") 'isearch-forward-use-region)

(global-set-key (kbd "C-c M-s g") 'helm-do-grep-ag)
;; (global-set-key (kbd "C-c M-s a g") 'counsel-ag)
(global-set-key (kbd "C-c M-s f") 'ag-files)
(global-set-key (kbd "C-x C-d") 'ag-dired-regexp)
(global-set-key (kbd "C-c M-f") 'ffap)

;; flyspell
(use-package flyspell-correct-helm
  :ensure t
  :init
  (setq flyspell-highlight-flag t)
  (setq flyspell-issue-message-flag nil)
  :config
  (add-hook 'text-mode-hook (lambda () (flyspell-mode 1)))
  (add-hook 'emacs-lisp-mode-hook (lambda () (flyspell-prog-mode)))
  (define-key flyspell-mode-map (kbd "C-c $") nil)
  ;; Remove C-, in flyspell in order to let embrace use it:
  (define-key flyspell-mode-map (kbd "C-,") nil)
  (define-key flyspell-mode-map (kbd "C-;") 'flyspell-correct-previous-word-generic))

;; worksapces setup:
(use-package setup-perspective)
;; windows with ace-window and avy
(use-package ace-window
  :ensure t
  :init
  (setq avy-background t)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq aw-dispatch-always nil)
  :bind
  ("M-o" . ace-window)
  ("M-g M-w" . avy-goto-word-1)
  ("M-g g" . avy-goto-line)
  ("M-g M-c" . avy-goto-char)
  ("M-g M-s" . avy-goto-char-in-line))

(use-package mode-icons
  :ensure t
  :config (mode-icons-mode))

(use-package ibuffer-vc
  :ensure t
  :init
  (defadvice ibuffer (around ibuffer-point-to-most-recent) ()
             "Open ibuffer with cursor pointed to most recent buffer name"
             (let ((recent-buffer-name (buffer-name)))
               ad-do-it
               (ibuffer-jump-to-buffer recent-buffer-name)))
  (global-set-key (kbd "C-x C-b") 'ibuffer)
  (setq ibuffer-expert t)
  (setq ibuffer-show-empty-filter-groups nil)
  :config
  (ad-activate 'ibuffer)
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-vc-set-filter-groups-by-vc-root)
              (ibuffer-auto-mode 1)
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


;; Rest Client:
(use-package restclient
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.client\\'" . restclient-mode)))

;; Mongo
(use-package inf-mongo
  :ensure t)

(use-package embrace
  :ensure t
  :init
  (global-set-key (kbd "C-,") 'embrace-commander))

(use-package nodejs-repl
  :ensure t)

(use-package golden-ratio-scroll-screen
  :ensure t
  :config
  (global-set-key [remap scroll-down-command] 'golden-ratio-scroll-screen-down)
  (global-set-key [remap scroll-up-command] 'golden-ratio-scroll-screen-up))

(use-package golden-ratio
  :ensure t
  :bind
  ("C-c g" . golden-ratio)
  ("C-c C-g" . balance-windows))

(use-package fancy-narrow
  :ensure t)

(use-package google-this
  :ensure t
  :bind
  ("C-c t" . google-this))

(use-package back-button
  :ensure t
  :bind
  ("M-]" . back-button-local-forward)
  ("M-[" . back-button-local-backward)
  ("C-M-{" . back-button-global-backward)
  ("C-M-}" . back-button-global-forward)
  :config
  (back-button-mode 1))

(use-package string-inflection
  :ensure t
  :bind
  ("C-c M-c" . string-inflection-lower-camelcase)
  ("C-c M-u" . string-inflection-underscore))

(provide 'init)
;;; init.el ends here
