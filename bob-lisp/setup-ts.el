(use-package typescript-mode
  :hook
  ;; (typescript-mode . tide-setup)
  ;; (typescript-mode . dap-mode)
  ;; (typescript-mode . highlight-indent-guides-mode)
  (typescript-mode . yas-minor-mode)
  (typescript-mode . origami-mode) 
  ;; (typescript-mode . eldoc-mode) 
  ;; (typescript-mode . bob/setup-typescript-flycheck)
  (typescript-mode . setup-tide-mode)
  :config
  (setq typescript-indent-level 2))

(use-package ts-comint :disabled t)

;; (defun bob/setup-typescript-flycheck ()
;;   (progn
;;     ;; (lsp)
;;     ;; (dap-mode)
;;     (add-node-modules-path)
;;     (flycheck-mode +1)
;;     ;; (lsp-diagnostics-mode)
;;     ;; (flycheck-select-checker 'javascript-eslint)
;;     ;; (flycheck-add-next-checker 'javascript-eslint 'lsp)
;;     (flycheck-add-next-checker 'javascript-tide 'javascript-eslint)
;;     )
;;   )

(provide 'setup-ts)
