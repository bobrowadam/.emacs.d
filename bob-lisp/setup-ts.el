(use-package typescript-mode
  :hook
  (typescript-mode . yas-minor-mode)
  (typescript-mode . origami-mode)
  (typescript-mode . setup-tide-mode)
  (typescript-mode . add-node-modules-path)
  ;; (typescript-mode . lsp)
  :config
  (setq typescript-indent-level 2)
  (flycheck-add-next-checker 'typescript-tide 'javascript-eslint)
  ;; (flycheck-add-next-checker 'lsp 'javascript-eslint)
  )


(use-package ts-comint
  :config
  (setq ts-comint-program-command "ts-node"))

(provide 'setup-ts)
