(use-package hl-line
  :disabled t
  :config
  (global-hl-line-mode))

(setq user-login-name "Adam Bobrow"
      undo-limit (* 1024 1000) ;; 1 Mb seems OK
      scroll-preserve-screen-position nil
      make-backup-files nil
      enable-recursive-minibuffers t
      inhibit-splash-screen t
      inhibit-startup-message t
      require-final-newline nil
      truncate-partial-width-windows 80
      sentence-end-double-space t       ; explicitly choose default
      x-select-enable-clipboard t
      set-mark-command-repeat-pop t
      mark-ring-max 6
      global-mark-ring-max 8
      history-delete-duplicates t
      comint-input-ignoredups t
      view-read-only nil          ; all read-only buffers in view-mode
      view-inhibit-help-message t ; don't tell me about it
      delete-active-region nil    ; just use <delete>
      gdb-many-windows t
      epa-pinentry-mode 'loopback
      auth-sources '("~/.authinfo" "~/.authinfo.gpg" "~/.netrc")
      dired-recursive-deletes 'always
      dired-recursive-copies 'always
      display-time-day-and-date t
      display-time-24hr-format t
      display-time-default-load-average nil
      shift-select-mode nil
      custom-file (expand-file-name "custom.el" user-emacs-directory)
      Info-additional-directory-list `(,(expand-file-name "info-docs" user-emacs-directory))
      ring-bell-function 'ignore
      visible-bell nil
      enable-local-variables :safe)

(setq browse-url-chrome-program "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome")

(when (boundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode 1))

(setq scroll-conservatively 101)

(unless (not (file-exists-p custom-file))
  (load custom-file))

(setq-default
 indent-tabs-mode nil)

(display-time)
(display-battery-mode)
(menu-bar-mode -1)
(column-number-mode 1)

(when (window-system)
  (setq confirm-kill-emacs 'yes-or-no-p))

(defalias 'yes-or-no-p 'y-or-n-p)

(when window-system
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1))

(blink-cursor-mode 0)
(global-subword-mode t)
(global-superword-mode -1)
(delete-selection-mode 1)
(set-default 'indent-tabs-mode nil)
(global-auto-revert-mode 1)
(auto-compression-mode t)
(global-word-wrap-whitespace-mode t)

;; delete char and delte word with "C-h" "C-M-h"
(define-key key-translation-map [?\C-h] [?\C-?])
(define-key key-translation-map (kbd "<f1>") (kbd "C-h"))
;; And also in Hebrew
(define-key key-translation-map [?\C-י] [?\C-?])
(global-unset-key (kbd "s-n"))

;; Deal with editing large files:
(global-so-long-mode 1)

;; Put backup files neatly away
(let ((backup-dir "~/tmp/emacs/backups")
      (auto-saves-dir "~/tmp/emacs/auto-saves/"))
  (dolist (dir (list backup-dir auto-saves-dir))
    (when (not (file-directory-p dir))
      (make-directory dir t)))
  (setq backup-directory-alist `(("." . ,backup-dir))
        auto-save-file-name-transforms `((".*" ,auto-saves-dir t))
        auto-save-list-file-prefix (concat auto-saves-dir ".saves-")
        tramp-backup-directory-alist `((".*" . ,backup-dir))
        tramp-auto-save-directory auto-saves-dir)
  (setq vc-handled-backends '(Git)))

(setq backup-by-copying t    ; Don't delink hardlinks
      delete-old-versions t  ; Clean up the backups
      version-control t      ; Use version numbers on backups,
      kept-new-versions 5    ; keep some new versions
      kept-old-versions 2)   ; and some old ones, too

(add-to-list 'prog-mode-hook #'display-line-numbers-mode)
(setq linum-format "%4d  ")

;; Set Emacs C source dir:
(setq find-function-C-source-directory "~/source/emacs/src")

(use-package which-key
  :demand t
  :if (window-system)
  :config
  (which-key-mode 1))

(use-package flyspell
  :bind  (:map flyspell-mode-map
               ("C-;" . nil)
               ("C-." . nil))
  :ensure nil
  :hook
  (minibuffer-mode . flyspell-mode)
  ;; (prog-mode . flyspell-mode)
  ;; (org-mode . flyspell-mode)
  ;; (git-commit-setup . git-commit-turn-on-flyspell
  ;; :config
  ;; (setq flyspell-issue-message-flag nil)
)

(use-package wucuo
  :commands (wucuo-start)
  :custom
  (wucuo-personal-font-faces-to-check '(tree-sitter-hl-face:string
                                        tree-sitter-hl-face:comment
                                        tree-sitter-hl-face:constant
                                        tree-sitter-hl-face:function
                                        tree-sitter-hl-face:variable
                                        git-commit-summary))
  :config
  (setq ispell-program-name "aspell")
  ;; You could add extra option "--camel-case" for camel case code spell checking if Aspell 0.60.8+ is installed
  ;; @see https://github.com/redguardtoo/emacs.d/issues/796
  (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--camel-case"))
  :hook
  (prog-mode . wucuo-start)
  (text-mode . wucuo-start)
  (web-mode . wucuo-start)
  (minibuffer-mode . wucuo-start)
  (git-commit-setup . wucuo-start))

(use-package emojify)
(use-package zoom-window :bind ("C-x C-z" . zoom-window-zoom))
(use-package iedit
  :bind ("C-;" . iedit-mode))
(use-package vundo
  :commands (vundo)
  :bind ("C-x u" . vundo))

;; On my mac I accidentally zoomed in and out with my palm
(global-unset-key (kbd "C-<wheel-up>"))
(global-unset-key (kbd "C-<wheel-down>"))

;; Map movement keys to Hebrew letters:
(global-set-key (kbd "C-כ") 'forward-char)
(global-set-key (kbd "C-נ") 'backward-char)
(global-set-key (kbd "C-מ") 'next-line)
(global-set-key (kbd "C-פ") 'previous-line)
(global-set-key (kbd "C-ש") 'beginning-of-line)
(global-set-key (kbd "C-ק") 'end-of-line)

(setq xref-search-program 'ripgrep)
(provide 'basic-settings)
