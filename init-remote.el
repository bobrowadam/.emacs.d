(setq lexical-binding t)
(setq gc-cons-threshold-before-init gc-cons-threshold)
(setq gc-cons-threshold 80000000)
(setq debug-on-error nil)
(setq package-enable-at-startup t)
(defconst remote-mode t)

(package-initialize)
(setq gc-cons-percentage 0.6)
(setq gc-cons-threshold 80000000)

;; (add-hook 'emacs-startup-hook
;;           (lambda ()
;;             (bob/set-gc-timer)))

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
  :disabled t
  :after startup
  :load-path "./modules"
  :demand t)

(use-package appearance
  :after startup
  :load-path "./modules"
  :demand t)

(use-package navigation
  :disabled t
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

(use-package setup-shell
  :after startup
  :demand t
  :load-path "./modules")

(put 'dired-find-alternate-file 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'list-timers 'disabled nil)
(put 'magit-clean 'disabled nil)
