(use-package typescript-mode
  :hook
  (typescript-mode . yas-minor-mode)
  (typescript-mode . origami-mode)
  (typescript-mode . setup-tide-mode)
  :config
  (setq typescript-indent-level 2)
  (flycheck-add-next-checker 'typescript-tide 'javascript-eslint))

(use-package ts-comint
  :config
  (setq ts-comint-program-command "ts-node"))

(provide 'setup-ts)
