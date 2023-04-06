;; (package-initialize)
(setq gc-cons-threshold 100000000)
(setq debug-on-error nil)
(setq package-enable-at-startup nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 10000000)
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

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
  :demand t
  :load-path "./modules")

(use-package setup-org
  :after startup
  :demand t
  :load-path "./modules")

(use-package setup-shell
  :after startup
  :demand t
  :load-path "./modules")

(use-package sicp)

(use-package riseup-helpers
  :demand t
  :ensure nil
  :after (startup magit))

(use-package gptel
  :config
  (setq gptel-api-key (exec-path-from-shell-copy-env "OPEN_AP_API_KEY"))
  (setq gptel-default-mode #'org-mode)
  (setq gptel-model "gpt-3.5-turbo-0301"))

(use-package gpt
  :ensure nil
  :load-path "~/source/gpt.el"
  :init
  (setq python-interpreter "python3.11")
  (setq gpt-openai-key (exec-path-from-shell-copy-env "OPEN_AP_API_KEY"))
  (setq gpt-openai-engine "gpt-3.5-turbo")
  :bind
  ("M-C-g" . gpt-dwim))
(put 'dired-find-alternate-file 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'magit-clean 'disabled nil)
