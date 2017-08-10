;;;; package --- Summary
;;; Commentary:
;; Keyboard General Setup
;;
;;; code:

;; Command as meta
(when (eq system-type 'darwin)
  (setq mac-right-option-modifier 'meta)
  (setq mac-command-modifier 'meta))

;; You need to realy want to quit:
(global-unset-key (kbd "C-x C-c"))
(global-set-key (kbd "C-x r q") 'save-buffers-kill-terminal)

;; Editing lines:
(global-set-key (kbd "C-c C-p") 'open-line-above)
(global-set-key (kbd "C-c C-n") 'open-line-below)
(global-set-key (kbd "C-c C-k") 'kill-and-retry-line)
(global-set-key (kbd "C-^") (λ (delete-indentation '-)))

;; Counsel
(global-set-key (kbd "M-y") 'counsel-yank-pop)

(global-set-key (kbd "C-h f") 'describe-function)
(global-set-key (kbd "C-h v") 'describe-variable)

;; Multiiple cursors:
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c C->") 'mc/mark-all-like-this)



;; Helm
(global-set-key (kbd "C-x f") 'helm-find-files)
(global-set-key (kbd "M-s ag") 'helm-do-grep-ag)
(global-set-key (kbd "C-x C-d") 'ag-dired-regexp)

;; Smex:
(use-package smex
  :ensure t
  :bind (( "M-x" . smex)
         ("M-X" . smex-major-mode-commands)
         ("C-c C-c M-x" . execute-extended-command)))

;; Windows
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-x C-]") 'delete-other-windows)
(global-set-key (kbd "C-}") 'enlarge-window-horizontally)
(global-set-key (kbd "C-{") 'shrink-window-horizontally)
(global-set-key (kbd "C-c g") 'golden-ratio)
(global-set-key (kbd "C-x 3") 'split-window-right-and-move-there-dammit)
(global-set-key (kbd "C-x k") 'kill-buffer-and-window)

;; Yas-snippets:
(global-set-key (kbd "C-c y e") 'yas-expand)
(global-set-key (kbd "C-c y i") 'yas-insert-snippet)

(provide 'key-bindings)
;;; key-bindings.el ends here
