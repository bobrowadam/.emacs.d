(use-package haskell-mode
  :hook (haskell-mode . flycheck-mode)
  )

(use-package dante
  :after haskell-mode
  :commands 'dante-mode
  :init
  :hook (haskell-mode . dante-mode)
  )

(provide 'haskell-setup)
