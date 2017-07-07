;;;; package --- Summary
;;; Commentary:
;;; Smart parens config
;;;
;;; code:

(use-package smartparens
  :ensure t
  :config
  ;; we use global mode and toggle off in setup-paredit.el for paredit related major-modes
  (smartparens-global-mode)
  (global-set-key (kbd "C-\)") 'sp-unwrap-sexp))
  
(provide 'setup-smartparens)

;;; setup-smartparens ends here
