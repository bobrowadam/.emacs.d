;;; package --- Summary ---  My first init.el file
;; Author: Bob Row
;;; Commentary:

;;; Code:
(package-initialize)

;;emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(setq initial-scratch-message "Greetings master Bob, welcome back.\nWhat shell we do today sir?")

;; add elpa and melpa repos
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
	("melpa" . "http://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")))

;;Show match numbers when searching
(global-anzu-mode +1)

;; Set path to dependencies
(setq settings-dir
      (expand-file-name "settings" user-emacs-directory))
(setq site-lisp-dir
      (expand-file-name "site-lisp" user-emacs-directory))

;; Set up load path
(add-to-list 'load-path settings-dir)
(add-to-list 'load-path site-lisp-dir)

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
(whole-line-or-region-mode 1)



;; helm
(require 'helm-config)

;; imenu lets you jump around for jumping to points of interest in a buffer
(global-set-key (kbd "M-i") 'imenu)

;; flycheck
(require 'flycheck)

;; javascript configuration:
;;;; js2 mode config:
(eval-after-load 'js2-mode
  '(add-hook 'js2-mode-hook #'add-node-modules-path 'ac-js2-mode))
(add-hook 'js2-mode-hook 'highlight-indent-guides-mode)
(add-hook 'js2-mode-hook 'js2-mode-hide-warnings-and-errors)

;; tern
(autoload 'tern-mode "tern.el" nil t)
(ac-config-default)
(add-hook 'js-mode-hook (lambda () (tern-mode t)))
(eval-after-load 'tern
  '(progn
     (require 'tern-auto-complete)
     (tern-ac-setup)))

;; (setq ac-js2-evaluate-calls t)
;; (require 'auto-complete)
(setq js-indent-level 2)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;;;; customize flycheck temp file prefix
(setq-default flycheck-temp-prefix ".flycheck")

;; this hopefully sets up path and other vars better
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))


;; use local eslint from node_modules before global
;;;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

;; eslint configuration:
(add-hook 'after-init-hook #'global-flycheck-mode)

;; disable jshint:
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(javascript-jshint)))

(setenv "path" (concat (getenv "path") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

;; magit configuratinon
(global-set-key (kbd "C-x g") 'magit-status)
;;;; magithub config:
(require 'magithub)
(magithub-feature-autoinject t)
(setq magithub-clone-default-directory "~/source/")

;; slime
(load "setup-common-lisp")

;; paredit
(load "setup-paredit")

;; smartparens
(load "setup-smartparens")
(load "setup-org")

;; projectile:
(projectile-mode)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(require 'which-key)
(which-key-mode +1)

(eval-after-load 'ido '(require 'setup-ido))
(eval-after-load 'dired '(require 'setup-dired))
(eval-after-load 'magit '(require 'setup-magit))
(eval-after-load 'grep '(require 'setup-rgrep))
(eval-after-load 'eshell '(require 'setup-eshell))
(eval-after-load 'prodigy '(require 'setup-pipeline))




(load "key-bindings")

;; visual regexp
;; (require 'visual-regexp)
;; (define-key global-map (kbd "m-%") 'vr/query-replace)
;; (define-key global-map (kbd "m-/") 'vr/replace)

;; Tramp
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


;;;; to connect via shadow:
;;;; /sshx:shadow|ssh:ubuntu@od-orenhazan|sudo:root@od-orenhazan:/
(add-to-list 'tramp-default-proxies-alist
             '("od-orenhazan" "\\`ubuntu\\'" "/sshx:shadow:")
             '("prod-einstein-1" "\\`ubuntu\\'" "/sshx:shadow:"))
(add-to-list 'tramp-restricted-shell-hosts-alist
             "\\shadow\\'")

;;Disable projectile mode line project naming for better performance:
(add-hook 'find-file-hook
          (lambda ()
            (when (file-remote-p default-directory)
              (setq-local projectile-mode-line "Projectile"))))

;; pdf - tools:
(pdf-tools-install)

;; (require 'expand-region)
(require 'multiple-cursors)
(require 'delsel)
(require 'jump-char)
(require 'eproject)
(require 'wgrep)
(require 'smart-forward)
(require 'change-inner)
(require 'multifiles)
(require 'smex)


;; Flex search mode
(require 'flex-isearch)

(global-flex-isearch-mode t)
(global-set-key (kbd "M-C-s") 'flex-isearch-forward)
(global-set-key (kbd "M-C-r") 'flex-isearch-backward)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)

;; ace jump config:
(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "ace jump back:-)"
  t)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))

;; Line numbers
;; highlight the current line number
(hlinum-activate)
(setq linum-format " %3d ")
;; turn on line numbers in prog-mode
(add-hook 'prog-mode-hook 'linum-mode)

;; flyspell
(require 'flyspell-correct-ido)
(define-key flyspell-mode-map (kbd "C-;") 'flyspell-correct-previous-word-generic)
(global-set-key (kbd "C-;") 'flyspell-correct-word-generic)

;; worksapces setup:
(require 'setup-perspective)

;; windows with ace-window
(require 'ace-window)
(global-set-key (kbd "C-x o") 'ace-window )
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
(setq aw-dispatch-always t)

(use-package pretty-mode
  :ensure t
  :config (add-hook 'js2-mode-hook 'turn-on-pretty-mode))

(use-package mode-icons
  :ensure t
  :config (mode-icons-mode))

(use-package slime
  :ensure t
  :config (setq inferior-lisp-program "/usr/local/bin/clisp"))

(provide 'init)
;;; init.el ends here
(put 'narrow-to-region 'disabled nil)
