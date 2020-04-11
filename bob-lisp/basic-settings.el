(setq make-backup-files nil
      enable-recursive-minibuffers t
      inhibit-splash-screen t
      require-final-newline nil
      truncate-partial-width-windows 80
      sentence-end-double-space t ; explicitly choose default
      x-select-enable-clipboard t
      set-mark-command-repeat-pop t

      history-delete-duplicates t
      comint-input-ignoredups t

      view-read-only t ; all read-only buffers in view-mode
      view-inhibit-help-message t ; don't tell me about it

      delete-active-region nil ; just use <delete>

      gdb-many-windows t

      epa-pinentry-mode 'loopback
      auth-sources '("~/.authinfo" "~/.authinfo.gpg" "~/.netrc")

      ;; No more damn prompts!
      dired-recursive-deletes 'always
      dired-recursive-copies 'always
      cursor-type '(bar . 4))

(setq initial-scratch-message ";; Oh it's you again :|")
(setq scroll-conservatively 10
      scroll-margin 2)
(setq display-time-day-and-date t)
(setq display-time-default-load-average nil)
(setq shift-select-mode nil)
(display-time)
(display-battery-mode)
(menu-bar-mode -1)
(when (window-system)
  (setq confirm-kill-emacs 'yes-or-no-p))

(when window-system
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1))

(global-subword-mode t)
(global-superword-mode -1)
(setq inhibit-startup-message t)
(setq ring-bell-function 'ignore
      visible-bell nil)
(setq-default indent-tabs-mode nil)

(delete-selection-mode 1)
(set-default 'indent-tabs-mode nil)
(global-auto-revert-mode 1)
(auto-compression-mode t)
(defalias 'yes-or-no-p 'y-or-n-p)

;; delete char and delte word with "C-h" "C-M-h"
(define-key key-translation-map [?\C-h] [?\C-?])
(define-key key-translation-map (kbd "<f1>") (kbd "C-h"))
;; (define-key key-translation-map (kbd "<f9>") (kbd "C-s-h"))

;; Theme and Font
(setq custom-safe-themes t)
(setq custom-theme-directory "~/.emacs.d/themes")
(set-frame-font "Monaco 21")
(add-to-list 'default-frame-alist
             '(font . "Monaco 21"))
;; (set-frame-font "Latin Modern Mono 21")
;; (add-to-list 'default-frame-alist
;;              '(font . "Latin Modern Mono 21"))
;; (load-theme 'bobs-badger t)

(use-package monokai-theme
  :demand t
  :config (load-theme 'monokai t))
 
(use-package doom-modeline
  :demand t
  :init
  (setq doom-modeline-height 20)
  (setq doom-modeline-bar-width 2)
  (setq doom-modeline-minor-modes nil)
  (setq doom-modeline-lsp t)
  (setq doom-modeline-github t)
  (setq doom-modeline-env-enable-rust t)
  (setq doom-modeline-env-rust-executable "rustc")
  (setq find-file-visit-truename t)
  (doom-modeline-mode 1))

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
        tramp-auto-save-directory auto-saves-dir))

(setq backup-by-copying t    ; Don't delink hardlinks
      delete-old-versions t  ; Clean up the backups
      version-control t      ; Use version numbers on backups,
      kept-new-versions 5    ; keep some new versions
      kept-old-versions 2)   ; and some old ones, too

(add-to-list 'prog-mode-hook #'linum-mode)
(setq linum-format "%4d   ")
;; Set Emacs C source dir:
(setq find-function-C-source-directory "~/source/emacs/src")

;; Ediff setup
(defmacro csetq (variable value)
  `(funcall (or (get ',variable 'custom-set)
                'set-default)
            ',variable ,value))
(csetq ediff-window-setup-function 'ediff-setup-windows-plain)
(csetq ediff-split-window-function 'split-window-horizontally)
(csetq ediff-diff-options "-w")

(use-package golden-ratio
  :init (defun my/gloden-ratio ()
          "Toggle golden ratio"
          (interactive)
          (if golden-ratio-mode
              (progn (golden-ratio-mode -1)
                     (balance-windows))
            (progn (golden-ratio-mode)
                   (golden-ratio))))
  :config (add-to-list 'golden-ratio-extra-commands 'ace-window))

(use-package undo-tree
  :disabled
  :demand t
  :config
  (global-undo-tree-mode 1))

(defun bob/create-scratch ()
  (interactive)
  (progn (get-buffer-create "*scratch*")
         (and initial-scratch-message
              (get-buffer "*scratch*")
              (with-current-buffer "*scratch*"
	        (when (zerop (buffer-size))
	          (insert (substitute-command-keys initial-scratch-message))
	          (set-buffer-modified-p nil))))
         (if (get-buffer "*scratch*")
             (with-current-buffer "*scratch*"
               (if (eq major-mode 'fundamental-mode)
	           (funcall initial-major-mode))))))

(provide 'basic-settings)
