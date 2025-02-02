(use-package hl-line)

(setq user-login-name "Adam Bobrow"
      user-full-name "Adam Bobrow"
      garbage-collection-messages nil
      undo-limit (* 30 1024 1000)
      scroll-preserve-screen-position t
      make-backup-files nil
      enable-recursive-minibuffers t
      inhibit-splash-screen t
      inhibit-startup-message t
      require-final-newline nil
      truncate-partial-width-windows 80
      sentence-end-double-space t       ; explicitly choose default
      x-select-enable-clipboard t
      set-mark-command-repeat-pop t
      mark-ring-max 30
      global-mark-ring-max 40
      history-delete-duplicates t
      comint-input-ignoredups t
      view-read-only nil          ; all read-only buffers in view-mode
      view-inhibit-help-message t ; don't tell me about it
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
      tab-width 4
      enable-local-eval t
      enable-local-variables t
      dictionary-server "dict.org")

;; Default was too low.
;; Increase for better lsp performance.
(setq read-process-output-max (* 3 1024 1024)) ;; 3mb

;; Default of 800 was too low.
;; Avoid Lisp nesting exceeding in swift-mode.
(setq max-lisp-eval-depth 10000)
(setq max-specpdl-size 10000)

(setq browse-url-chrome-program "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome")

;; FrogFind is a simple search engine for text based browsers
(setq eww-search-prefix "http://frogfind.com/?q=")

(when (boundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode 1))

(setq scroll-conservatively 10)
(setq scroll-margin 6)

(unless (not (file-exists-p custom-file))
  (load custom-file))

(setq-default indent-tabs-mode nil)

(display-time-mode -1)
(display-battery-mode -1)
(menu-bar-mode -1)
(line-number-mode 1)
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

(use-package jinx
  :custom
  (jinx-include-faces '((prog-mode font-lock-variable-name-face
                                   font-lock-comment-face
                                   font-lock-doc-face
                                   font-lock-string-face
                                   git-commit-summary)
                        (conf-mode font-lock-comment-face font-lock-string-face)
                        (yaml-mode . conf-mode)
                        (yaml-ts-mode . conf-mode)))
  :config
  (add-to-list 'jinx-camel-modes 'tsx-ts-mode)
  (add-to-list 'jinx-camel-modes 'roc-ts-mode)
  (defun jinx--load-dicts ()
    "Load dictionaries and setup syntax table."
    (setq jinx--dicts (delq nil (mapcar #'jinx--mod-dict
                                        (split-string jinx-languages)))
          jinx--syntax-table (make-syntax-table jinx--base-syntax-table))
    (unless jinx--dicts
      (message "Jinx: No dictionaries available for %S" jinx-languages))
    (dolist (dict jinx--dicts)
      (cl-loop for c across (jinx--mod-wordchars dict) do
               (modify-syntax-entry c "w" jinx--syntax-table)))
    (modify-syntax-entry ?' "." jinx--syntax-table)
    (modify-syntax-entry ?’ "w" jinx--syntax-table)
    (modify-syntax-entry ?. "." jinx--syntax-table))
  :hook (emacs-startup . global-jinx-mode)
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages)))

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

(global-unset-key (kbd "C-M-<mouse-5>"))
(global-unset-key (kbd "C-M-<mouse-4>"))
(global-unset-key (kbd "C-M-<wheel-down>"))
(global-unset-key(kbd "C-M-<wheel-up>"))

;; Map movement keys to Hebrew letters:
(global-set-key (kbd "C-כ") 'forward-char)
(global-set-key (kbd "C-נ") 'backward-char)
(global-set-key (kbd "C-מ") 'next-line)
(global-set-key (kbd "C-פ") 'previous-line)
(global-set-key (kbd "C-ש") 'beginning-of-line)
(global-set-key (kbd "C-ק") 'end-of-line)
(global-set-key (kbd "C-c T") 'display-time-mode)


;; scroll pages conservatively
(defun consertive-page-up ()
  "Scroll up half a page and try to preserve cursor position."
  (interactive)
  (let ((line-num (count-lines (window-start) (point))))
    (scroll-down-command (round (* (window-body-height) 0.25)))
    (move-to-window-line line-num)))

(defun consertive-page-down ()
  "Scroll down half a page and try to preserve cursor position."
  (interactive)
  (let ((line-num (count-lines (window-start) (point))))
    (scroll-up-command (round (* (window-body-height) 0.25)))
    (move-to-window-line line-num)))

(global-set-key (kbd "C-v") 'consertive-page-down)
(global-set-key (kbd "M-v") 'consertive-page-up)

(setq xref-search-program 'ripgrep)

(setq python-shell-interpreter (executable-find "python3.12"))

(provide 'basic-settings)
