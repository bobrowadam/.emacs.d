;;;; package --- Summary
;;; Commentary:
;; customo key bindings
;;
;;; code:

;; delete with C-h (help with C-S h)
;; (global-set-key (kbd "C-h") 'backward-delete-char-untabify)
;; (global-set-key (kbd "C-H") 'help)

;; command as meta
(when (eq system-type 'darwin)
  (setq mac-right-option-modifier 'meta)
  (setq mac-command-modifier 'meta))

;; you need to realy want to quit:
(global-unset-key (kbd "C-x C-c"))
(global-set-key (kbd "C-x r q") 'save-buffers-kill-terminal)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; editing lines:
(global-set-key (kbd "C-c C-p") 'open-line-above)
(global-set-key (kbd "C-c C-n") 'open-line-below)
(global-set-key (kbd "C-c C-j") 'open-line-and-indent)
(global-set-key (kbd "C-c C-w") 'kill-and-retry-line)
(global-set-key (kbd "C-^") (λ (delete-indentation '-)))

;; swiper:
;; (ivy-mode 1)
;; (setq ivy-use-virtual-buffers t)
;; (global-set-key "\C-s" 'swiper)
;; (global-set-key (kbd "C-c C-r") 'ivy-resume)
;; (global-set-key (kbd "M-x") 'counsel-M-x)
;; (global-set-key (kbd "C-x f") 'counsel-find-file)
;; (global-set-key (kbd "C-h f") 'counsel-describe-function)
;; (global-set-key (kbd "C-h v") 'counsel-describe-variable)
;; (global-set-key (kbd "<f1> l") 'counsel-load-library)
;; (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
;; (global-set-key (kbd "<f2> u") 'counsel-unicode-char)

(global-set-key (kbd "C-c c g") 'counsel-git)
(global-set-key (kbd "C-c c j") 'counsel-git-grep)
(global-set-key (kbd "C-c c k") 'counsel-ag)
(global-set-key (kbd "C-c c l") 'counsel-locate)
(global-set-key (kbd "M-y") 'counsel-yank-pop)
(global-set-key (kbd "C-h f") 'describe-function)
(global-set-key (kbd "C-h v") 'describe-variable)

;; smartparens keybindings:
(global-set-key (kbd "C-\)") 'sp-unwrap-sexp)

;; multiiple cursors:
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c C->") 'mc/mark-next-like-this-word)

;; helm
;; (global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "C-x f") 'helm-find-files)
(global-set-key (kbd "M-s ag") 'helm-do-grep-ag)
(global-set-key (kbd "C-x C-d") 'ag-dired-regexp)
;; (global-set-key (kbd "M-x") 'helm-M-x)

;; smex:
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; windows
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-x C-]") 'delete-other-windows)
(global-set-key (kbd "C-}") 'enlarge-window-horizontally)
(global-set-key (kbd "C-{") 'shrink-window-horizontally)
(global-set-key (kbd "C-c g") 'golden-ratio)
(global-set-key (kbd "C-x 3") 'split-window-right-and-move-there-dammit)

;; ace jump keys:
(define-key global-map (kbd "C-c j SPC") 'ace-jump-mode-pop-mark)
(define-key global-map (kbd "C-c j j") 'ace-jump-char-mode)
(define-key global-map (kbd "C-c j k") 'ace-jump-word-mode)

;; frames:
(global-set-key (kbd "C-x o") 'other-frame)

;; yas-snippets:
(global-set-key (kbd "C-c y e") 'yas-expand)
(global-set-key (kbd "C-c y i") 'yas-insert-snippet)

;;; hippie expand:
(define-key global-map (kbd "C-c r") 'helm-eshell-history)


;;eshel
(global-set-key [remap dabbrev-expand] 'hippie-expand)
(global-unset-key (kbd "C-\\"))
(global-set-key (kbd "C-\\") 'shell-pop)


;; Org
(define-key global-map (kbd "C-c o c") 'org-capture)
(define-key global-map (kbd "C-c o n") 'org-go-to-notes)
(define-key global-map (kbd "C-c o a") 'org-agenda)
(define-key global-map (kbd "C-c o t") 'org-go-to-todos)

(provide 'key-bindings)
;;; key-bindings.el ends here

