(setq lexical-binding t)
(setq gc-cons-threshold-before-init gc-cons-threshold)
(setq gc-cons-threshold 10000000)
(setq debug-on-error nil)
(setq package-enable-at-startup nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold gc-cons-threshold-before-init)
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(setq package-native-compile t)
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

(use-package setup-magit
  :after startup
  :defer 10
  :load-path "./modules")

(use-package setup-org
  :after startup
  :demand t
  :load-path "./modules")

(use-package setup-shell
  :defer 10
  :after startup
  :demand t
  :load-path "./modules")

(use-package sicp)

(use-package riseup-helpers
  :demand t
  :ensure nil
  :after (startup magit))

(use-package shell-maker
  :demand t
  :straight (:host github :repo "xenodium/chatgpt-shell" :files ("shell-maker.el")))

(use-package pcsv
  :demand t)

(use-package chatgpt-shell
  :defer 10
  :after shell-maker pcsv
  :straight (:host github :repo "xenodium/chatgpt-shell" :files ("chatgpt-shell.el"))
  :config
  (setq chatgpt-shell-openai-key (exec-path-from-shell-copy-env "OPEN_AP_API_KEY"))
  :bind
  ("C-c g" . chatgpt-shell))

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
  :demand t
  :straight (:host github :repo "elizagamedev/xkcd-303-mode.el"
                 :files ("*.el" "compiling.png"))
  :init
  (xkcd-303-mode 1))

(use-package mongo-cli
  :demand t
  :ensure nil
  :load-path "~/source/mongo-cli-el/")

(use-package proced-narrow
  :demand t
  :hook (proced-mode . proced-narrow-mode)
  :bind (:map proced-mode-map ("N" . 'proced-narrow)))

(use-package xwwp
  :bind (:map xwidget-webkit-mode-map
              ("C-x C--" . xwidget-webkit-zoom-out)
              ("C-x C-=" . xwidget-webkit-zoom-in)
              ("f" . xwwp-follow-link)))

(put 'dired-find-alternate-file 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'list-timers 'disabled nil)
(put 'magit-clean 'disabled nil)
