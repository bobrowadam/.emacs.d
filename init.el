(setenv "LIBRARY_PATH"
	(string-join
	 '("/opt/homebrew/opt/libgccjit/lib/gcc/14/"
	   "/opt/homebrew/opt/gcc/lib/gcc/13/gcc/aarch64-apple-darwin22/13")
	 ":"))
(setq debug-on-error nil)
(setq lexical-binding t)

(setq package-enable-at-startup nil)

(setq initial-buffer-choice t)
(setq initial-scratch-message nil)
(setq garbage-collection-messages t)

(defface init-title
  '((t :inherit info-title-3 :height 300))
  "A face For the initial Emacs title.")

(setq gc-cons-percentage-before-init gc-cons-percentage)
(setq gc-cons-threshold-before-init gc-cons-threshold)
(setq gc-cons-percentage 0.6)
(setq gc-cons-threshold (* gc-cons-threshold 1000))
(add-hook 'emacs-startup-hook
          (lambda ()
            (insert (propertize "M-x"
                                'face '(init-title)))
            (insert "\n\n ")
            (insert (propertize (format "Ready in %s with %d garbage collections.\nGC elapsed: %s"
                                        (format "%.2f seconds"
                                                (float-time
                                                 (time-subtract after-init-time before-init-time)))
                                        gcs-done
                                        (format "%.2f seconds" gc-elapsed))
                                'face '(info-title-1)))
            (setq gc-cons-percentage gc-cons-percentage-before-init)
            (setq gc-cons-threshold gc-cons-threshold-before-init)))

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("gnu-devel" . "https://elpa.gnu.org/devel/")))

(setq package-native-compile t)
(setq use-package-always-defer t)
(setq use-package-always-ensure t)
(setq use-package-verbose t)
(setq use-package-debug t)
(setq use-package-compute-statistics t)
;; use-package-enable-imenu-support must be
;; set before requiring use-package.
(setq use-package-enable-imenu-support t)

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

(use-package completions
  :after startup
  :demand t
  :load-path "./modules")

(use-package files-and-buffers
  :after startup
  :demand t
  :load-path "./modules")

(use-package prog
  :after (startup completions)
  :demand t
  :load-path "./modules")

(use-package setup-tree-sitter
  :ensure nil
  :demand t
  :load-path "./modules")

(use-package setup-magit
  :after startup
  :demand t
  :load-path "./modules")

(use-package setup-org
  :after startup
  :demand t
  :load-path "./modules"
  :bind
  ("C-c a" . org-agenda)
  ("C-c c" . org-capture)
  ("C-c s a" . run-cl-asana)
  ("C-c s c" . run-calendar-sync))

(use-package setup-shell
  :demand t
  :after startup
  :load-path "./modules")

(use-package sicp)

(use-package riseup-helpers
  :disabled t
  :commands (browse-riseup-git-project
             clone-riseup-repo import-customer
             browse-customer-in-mamadmin browse-customer-merge-in-mamadmin
             browse-data-dog-dwim run-customer-version
             search-for-riseup-service-by-port init-riseup-actions
             init-riseup-actions riseup-actions is-typescript-project)
  :config
  (init-riseup-actions)
  :bind
  ("C-c J" . riseup-actions)
  :ensure nil
  :after (startup))

(use-package shell-maker
  :straight (:host github :repo "xenodium/chatgpt-shell" :files ("shell-maker.el")))

(use-package pcsv
  :demand t)

(use-package chatgpt-shell
  :commands (chatgpt-shell chatgpt-shell-start)
  :custom
  (chatgpt-shell-model-version 0)
  :config
  (setq chatgpt-shell-openai-key (exec-path-from-shell-copy-env "OPEN_AP_API_KEY"))

  :bind
  ("C-c g" . chatgpt-shell)
  :hook
  (chatgpt-shell-mode . (lambda () (corfu-mode -1))))

(use-package claude-shell
  :commands (chatgpt-shell chatgpt-shell-start claude-shell)
  :custom
  (claude-shell-streaming t)
  ;; :after shell-maker pcsv
  :config
  (setq claude-shell-api-token (exec-path-from-shell-copy-env "CLAUDE_SHELL_API_TOKEN"))
  :bind
  ("C-c C-g" . claude-shell)
  :hook
  (claude-shell-mode . (lambda () (corfu-mode -1))))

(use-package breadcrumb-mode
  :straight (breadcrumb-mode :type git :host github :repo "joaotavora/breadcrumb")
  :init (breadcrumb-mode 1))

(use-package string-inflection)

(use-package khoj
  :disabled t
  :ensure t
  :pin melpa-stable
  ;; :custom
  ;; (khoj-server-python-command "python3.11")
  ;; (khoj-server-command "khoj")
  :config (setq khoj-org-directories `(,(s-chop-suffix "/" org-directory))
                khoj-openai-api-key (getenv "OPEN_AP_API_KEY")))

(use-package xkcd-303-mode
  :straight (:host github :repo "elizagamedev/xkcd-303-mode.el"
                 :files ("*.el" "compiling.png"))
  :config
  (xkcd-303-mode 1))

(use-package proced-narrow
  :demand t
  :hook (proced-mode . proced-narrow-mode)
  :bind (:map proced-mode-map ("N" . 'proced-narrow)))

(use-package xwwp
  :bind (:map xwidget-webkit-mode-map
              ("C-x C--" . xwidget-webkit-zoom-out)
              ("C-x C-=" . xwidget-webkit-zoom-in)
              ("f" . xwwp-follow-link)))

(use-package uuid
  :commands uuid-create)

(use-package impostman)

(use-package npm-utils
  :commands (bob/update-node-modules-if-needed-sync)
  :ensure nil)

(use-package grain-utils
  :commands (grain/run-service)
  :ensure nil)

(use-package casual-calc
  :bind
  (:map calc-mode-map
              ("C-o" . #'casual-calc-tmenu))
  (:map calc-alg-map
              ("C-o" . #'casual-calc-tmenu)))

(put 'dired-find-alternate-file 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'list-timers 'disabled nil)
(put 'magit-clean 'disabled nil)
