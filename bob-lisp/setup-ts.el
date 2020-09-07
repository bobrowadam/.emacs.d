(use-package typescript-mode
  :hook
  (typescript-mode . lsp)
  (typescript-mode . highlight-indent-guides-mode)
  (typescript-mode . yas-minor-mode)
  (typescript-mode . (lambda () (flycheck-mode +1)))
  (typescript-mode . origami-mode)
  (typescript-mode . (lambda () (eldoc-mode +1)))
  ;; (typescript-mode . dap-mode)
  (typescript-mode . (lambda ()
                       (progn
                         (add-node-modules-path)
                         (flycheck-select-checker 'javascript-eslint))))
  :bind
  (:map typescript-mode-map ("C-=" . origami-toggle-node))
  :config
  (flycheck-add-mode 'javascript-eslint 'typescript-mode)
  ;; (flycheck-add-mode 'typescript-tide 'typescript-mode)
  (setq typescript-indent-level 2)
  ;; (flycheck-add-next-checker 'javascript-eslint '(t . typescript-tide) 'append)
  (flycheck-add-next-checker 'javascript-eslint '(t . lsp) 'append)
  ;; (flycheck-add-next-checker 'javascript-eslint 'typescript-tide)
  )

(use-package ts-comint :disabled t)

(provide 'setup-ts)
