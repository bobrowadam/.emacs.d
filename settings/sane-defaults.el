;;; package --- Summary ---  saner defualts inspired by magnar
;; Author: Bob Row
;;; Commentary:

;;; Code:
(package-initialize)

;; You need to realy want to quit:
(global-unset-key (kbd "C-x C-c"))
(global-set-key (kbd "C-x r q") 'save-buffers-kill-terminal)

;; Delete with C-h and M-h instead of backspace
   ;; (global-set-key (kbd "s-h") 'help-command)
   ;; (global-set-key (kbd "C-M-?") 'mark-paragraph)
   ;; (global-set-key (kbd "C-h") 'delete-backward-char)
   ;; (global-set-key (kbd "M-h") 'backward-kill-word)

(global-set-key (kbd "M-h") 'backward-kill-word)

;; Command as meta
(when (eq system-type 'darwin)
  (setq mac-right-option-modifier 'meta)
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'control))

;; no startup message:
(setq inhibit-startup-message t)

;; no tool bar:
(tool-bar-mode -1)

;; blinking cursor:
(blink-cursor-mode -1)

;; no blips and blops
(setq ring-bell-function 'ignore
      visible-bell nil)

;; word jumping:
(global-subword-mode nil)
(global-superword-mode -1)

;; Allow pasting selection outside of Emacs
(setq select-enable-clipboard t)

;; show line numbers where they should:
(use-package linum-off
  :config
  (global-linum-mode 1))

(global-set-key (kbd "C-x f") 'helm-find-files)

;; show parens
(show-smartparens-global-mode t)

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Show keystrokes in progress
(setq echo-keystrokes 0.1)

;; Move files to trash when deleting
(setq delete-by-moving-to-trash t)

;; Real emacs knights don't use shift to mark things
(setq shift-select-mode nil)

;; Transparently open compressed files
(auto-compression-mode t)

;; Answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

;; Smex:
(use-package smex
  :ensure t
  :bind (( "M-x" . smex)
         ("M-X" . smex-major-mode-commands)
         ("C-c C-c M-x" . execute-extended-command)))

;; UTF-8 please
(setq locale-coding-system 'utf-8) ; pretty
(set-terminal-coding-system 'utf-8) ; pretty
(set-keyboard-coding-system 'utf-8) ; pretty
(set-selection-coding-system 'utf-8) ; please
(prefer-coding-system 'utf-8) ; with sugar on top

;; Show active region
(transient-mark-mode 1)
(make-variable-buffer-local 'transient-mark-mode)
(put 'transient-mark-mode 'permanent-local t)
(setq-default transient-mark-mode t)

;; Remove text in active region if inserting text
(delete-selection-mode 1)

;; Always display line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)

;; Lines should be 80 characters wide, not 72
(setq fill-column 80)

;; Save a list of recent files visited. (open recent file with C-x f)
(recentf-mode 1)
(setq recentf-max-saved-items 100) ;; just 20 is too recent

;; Save minibuffer history
(savehist-mode 1)
(setq history-length 1000)

;; Never insert tabs
(set-default 'indent-tabs-mode nil)
(setq-default tab-width 2)

;; Cleanup whitespaces
(global-set-key (kbd "C-x j")'whitespace-cleanup)

;; Show me empty lines after buffer end
(set-default 'indicate-empty-lines t)

;; Keep cursor away from edges when scrolling up/down
(use-package smooth-scrolling
 :ensure t
 :config (smooth-scrolling-mode t))

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;; Allow recursive minibuffers
(setq enable-recursive-minibuffers t)

;; Don't be so stingy on the memory, we have lots now. It's the distant future.
(setq gc-cons-threshold 20000000)

;; Fontify org-mode code blocks
(setq org-src-fontify-natively t)

;; Represent undo-history as an actual tree (visualize with C-x u)
(use-package undo-tree
  :disabled
  :ensure t
  :init (setq undo-tree-mode-lighter "")
  :config (global-undo-tree-mode))

;; Sentences do not need double spaces to end. Period.
(set-default 'sentence-end-double-space nil)

;; 80 chars is a good width.
(set-default 'fill-column 80)

;; Add parts of each file's directory to the buffer name if not unique
(use-package uniquify
  :init
  (setq uniquify-buffer-name-style 'forward))

;; A saner ediff
(setq ediff-diff-options "-w")
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; No electric indent
(setq electric-indent-mode nil)

;; When popping the mark, continue popping until the cursor actually moves
;; Also, if the last command was a copy - skip past all the expand-region cruft.
(defadvice pop-to-mark-command (around ensure-new-position activate)
  (let ((p (point)))
    (when (eq last-command 'save-region-or-current-line)
      ad-do-it
      ad-do-it
      ad-do-it)
    (dotimes (i 10)
      (when (= p (point)) ad-do-it))))

(setq set-mark-command-repeat-pop t)

;; Offer to create parent directories if they do not exist
;; http://iqbalansari.github.io/blog/2014/12/07/automatically-create-parent-directories-on-visiting-a-new-file-in-emacs/
(defun my-create-non-existent-directory ()
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p "? " (format "Directory `%s' does not exist! Create it?" parent-directory)))
      (make-directory parent-directory t))))

(add-to-list 'find-file-not-found-functions 'my-create-non-existent-directory)

;;Show match numbers when searching
(use-package anzu
  :ensure t
  :config
  (global-anzu-mode +1))

;; Editing lines:
(global-set-key (kbd "C-c C-p") 'open-line-above)
(global-set-key (kbd "C-c C-n") 'open-line-below)
(global-set-key (kbd "C-c C-k") 'kill-and-retry-line)
(global-set-key (kbd "C-^") (λ (delete-indentation '-)))

;; Windows
;; (global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-x C-]") 'delete-other-windows)
(global-set-key (kbd "C-}") 'enlarge-window-horizontally)
(global-set-key (kbd "C-{") 'shrink-window-horizontally)

(global-set-key (kbd "C-x 3") 'split-window-right-and-move-there-dammit)
(global-set-key (kbd "C-x k") 'kill-buffer-and-window)


(provide 'sane-defaults)
;;; sane-defaults ends here
