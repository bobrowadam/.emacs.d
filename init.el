(setenv "LIBRARY_PATH"
	(string-join
	 '("/opt/homebrew/opt/libgccjit/lib/gcc/14/"
	   "/opt/homebrew/opt/gcc/lib/gcc/13/gcc/aarch64-apple-darwin22/13")
	 ":"))
(setq debug-on-error nil)
(setq lexical-binding t)

(setq package-enable-at-startup nil)

(setq initial-buffer-choice t)
(setq garbage-collection-messages t)

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

(use-package setup-llm
  :demand t
  :after startup
  :load-path "./modules")

(use-package sicp)

(use-package pcsv
  :demand t)

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
  :commands (uuid-create uuid-string))

(use-package impostman)

(use-package npm-utils
  :commands (bob/update-node-modules-if-needed-sync is-typescript-project)
  :ensure nil)

(use-package grain-utils
  :commands (grain/run-service)
  :ensure nil)

(use-package jest-utils
  :commands (bob/jest-run-tests bob/jest-rerun-latest-test)
  :ensure nil
  :bind
  (:map typescript-ts-base-mode-map
   ("C-c C-t C-n" . #'bob/jest-run-tests)
   ("C-c C-t C-r" . #'bob/jest-rerun-latest-test)))

(use-package casual-calc
  :bind
  (:map calc-mode-map
              ("C-o" . #'casual-calc-tmenu))
  (:map calc-alg-map
              ("C-o" . #'casual-calc-tmenu)))

(use-package pomm
  :custom
  (pomm-audio-enabled t)
  (pomm-audio-tick-enabled nil)
  :commands (pomm pomm-third-time))

(use-package markdown-mode
  :custom
  (markdown-enable-highlighting-syntax t)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)
         ("\\.mess\\'" . markdown-mode))
  :init (setq markdown-command "pandoc"))

(use-package multiple-cursors
  :bind
  ("C-S-c C-c" . mc/edit-lines)
  ("C-S-c C->" . mc/mark-next-like-this)
  ("C-S-c C-<" . mc/mark-previous-like-this)
  ("C-S-c C-." . mc/mark-all-like-this))

(put 'dired-find-alternate-file 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'list-timers 'disabled nil)
(put 'magit-clean 'disabled nil)
(put 'magit-edit-line-commit 'disabled nil)
