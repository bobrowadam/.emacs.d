;; -*- lexical-binding: t; -*-

(setq package-enable-at-startup nil)

;; GUI Emacs does not inherit user-local, Homebrew, Cargo, or Pyenv paths
;; from the login shell.
(let* ((directories (delq nil
                          (mapcar (lambda (directory)
                                    (when (file-directory-p directory)
                                      directory))
                                  (list (expand-file-name "~/.local/bin")
                                        (expand-file-name "~/.pyenv/shims")
                                        "/opt/homebrew/bin"
                                        (expand-file-name "~/.cargo/bin")))))
       (separator (if (characterp path-separator)
                      (char-to-string path-separator)
                    path-separator))
       (path (split-string (or (getenv "PATH") "") separator t)))
  ;; Prepend in declared order, removing existing entries first.
  (dolist (directory (reverse directories))
    (setq exec-path (cons directory (delete directory exec-path)))
    (setq path (cons directory (delete directory path))))
  (when directories
    (setenv "PATH" (mapconcat #'identity path separator))))

;; Match theme color early on (smoother transition).
(add-to-list 'default-frame-alist '(background-color . "#1f1f28"))
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

(unless (daemonp)
  (advice-add #'display-startup-echo-area-message :override #'ignore))

(setq frame-inhibit-implied-resize t)
(setq inhibit-compacting-font-caches t)

;; No scrollbar by default.
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; No nenubar by default.
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))

;; No toolbar by default.
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; No tooltip by default.
(when (fboundp 'tooltip-mode)
  (tooltip-mode -1))

(setq inhibit-startup-screen t)

;; Save original GC values
(defvar bob/gc-cons-percentage-original gc-cons-percentage
  "Original value of `gc-cons-percentage'.")
(defvar bob/gc-cons-threshold-original gc-cons-threshold
  "Original value of `gc-cons-threshold'.")

;; Set high GC threshold during startup for faster loading
(setq gc-cons-percentage 0.6)
(setq gc-cons-threshold (* 100 1024 1024))

;; Restore after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-percentage bob/gc-cons-percentage-original)
            (setq gc-cons-threshold bob/gc-cons-threshold-original)
            (message "Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds" (float-time (time-subtract after-init-time before-init-time)))
                     gcs-done)))
