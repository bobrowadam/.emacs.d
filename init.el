(setq lexical-binding t)

(setq debug-on-error nil)
(setq package-enable-at-startup nil)

(setq initial-buffer-choice t)
(setq initial-scratch-message nil)

(setq gc-cons-percentage 0.6)
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda ()
            (bob/set-gc-timer)
            (insert (format "Emacs ready in %s with %d garbage collections.\nGC elapsed: %s"
                            (format "%.2f seconds"
                                    (float-time
                                     (time-subtract after-init-time before-init-time)))
                            gcs-done
                            gc-elapsed))
            (let ((animated-string "I use emacs BTW"))
                (animate-string animated-string
                             3
                             0))))

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
  ("C-c s a" . run-cl-asana))

(use-package setup-shell
  :demand t
  :after startup
  :load-path "./modules")

(use-package sicp)

(use-package riseup-helpers
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
  :demand t
  :straight (:host github :repo "xenodium/chatgpt-shell" :files ("shell-maker.el")))

(use-package pcsv
  :demand t)

(use-package chatgpt-shell
  :commands (chatgpt-shell chatgpt-shell-start)
  :after shell-maker pcsv
  :custom
  (chatgpt-shell-model-version 6)
  (chatgpt-shell-model-versions '("gpt-3.5-turbo"
                                  "gpt-3.5-turbo-0613"
                                  "gpt-3.5-turbo-16k"
                                  "gpt-3.5-turbo-16k-0613"
                                  "gpt-4"
                                  "gpt-4-0613"
                                  "gpt-4-1106-preview"))
  :straight (:host github :repo "xenodium/chatgpt-shell" :files ("chatgpt-shell.el"))
  :config
  (setq chatgpt-shell-openai-key (exec-path-from-shell-copy-env "OPEN_AP_API_KEY"))

  :bind
  ("C-c g" . chatgpt-shell)
  :hook
  (chatgpt-shell-mode . (lambda () (corfu-mode -1))))

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

(use-package uuid
  :commands uuid-create)

(put 'dired-find-alternate-file 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'list-timers 'disabled nil)
(put 'magit-clean 'disabled nil)
