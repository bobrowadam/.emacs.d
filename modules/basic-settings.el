;;; basic-settings.el --- summary -*- lexical-binding: t -*-
;;; Commentary:

;; commentary

;;; Code:
;; Default was too low.
;; Increase for better lsp performance.
(setq read-process-output-max (* 3 1024 1024)) ;; 3mb
(setq browse-url-chrome-program "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome")
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
      Info-additional-directory-list `(,(expand-file-name "info-docs" user-emacs-directory))
      ring-bell-function 'ignore
      visible-bell nil
      tab-width 4
      enable-local-eval t
      enable-local-variables t
      dictionary-server "dict.org"
      tab-always-indent 'complete
      text-mode-ispell-word-completion nil)

(setq read-extended-command-predicate #'command-completion-default-include-p)
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))

;; FrogFind is a simple search engine for text based browsers
(setq eww-search-prefix "http://frogfind.com/?q=")

(when (boundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode 1))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(unless (not (file-exists-p custom-file))
  (load custom-file))

(setq-default indent-tabs-mode nil)

(when (window-system)
  (setq confirm-kill-emacs 'yes-or-no-p))

(defalias 'yes-or-no-p 'y-or-n-p)

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

(setq linum-format "%4d  ")

;; Set Emacs C source dir:
(setq find-function-C-source-directory "~/source/emacs/src")

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
(defun consertive-page-upδ ()
  "Scroll up half a page and try to preserve cursor position."
  (interactive)
  (let ((line-num (count-lines (window-start) (point))))
    (scroll-down-command (round (* (window-body-height) 0.25)))
    (move-to-window-line line-num)))

(defun consertive-page-downδ ()
  "Scroll down half a page and try to preserve cursor position."
  (interactive)
  (let ((line-num (count-lines (window-start) (point))))
    (scroll-up-command (round (* (window-body-height) 0.25)))
    (move-to-window-line line-num)))

(global-set-key (kbd "C-v") 'consertive-page-downδ)
(global-set-key (kbd "M-v") 'consertive-page-upδ)

(defun scroll-forward-lineδ ()
  "Scroll  forward a single line."
  (interactive)
  (forward-line 1)
  (recenter))
(defun scrol-backward-lineδ ()
  "Scroll backward a single line."
  (interactive)
  (forward-line -1)
  (recenter))
(global-set-key (kbd "C-s-p") 'scrol-backward-lineδ)
(global-set-key (kbd "C-s-n") 'scroll-forward-lineδ)
(global-set-key (kbd "C-x 8 l")
                (lambda ()
                  (interactive (insert "𝝺"))))
(global-set-key (kbd "C-x 8 d")
                (lambda ()
                  (interactive (insert "δ"))))

(setq xref-search-program 'ripgrep)

(setq python-shell-interpreter (executable-find "python3.12"))

(setq custom-safe-themes t)
(setq custom-theme-directory (concat user-emacs-directory "themes"))

(let ((calculated-font-size
       (if (>= (nth 3 (assoc 'geometry (frame-monitor-attributes))) 1920)
           25
         23)))
  ;; (set-frame-font (format "Iosevka-%d:weight=medium:width=expanded" calculated-font-size)
  ;;                 'keep-size t)
  (set-frame-font (format "Aporetic Sans Mono-%d" calculated-font-size)
                  'keep-size t))
(setq kill-buffer-query-functions nil)

;; Trust the ~/source/ file so flymake byte compile will work.
(add-to-list 'trusted-content "~/source/")

(provide 'basic-settings)

;;; basic-settings.el ends here
