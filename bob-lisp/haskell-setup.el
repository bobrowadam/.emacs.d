(use-package haskell-mode
  :demand t
  :hook (haskell-mode . flycheck-mode)
  )

(use-package dante
  :demand t
  :after haskell-mode
  :commands 'dante-mode
  :init
  :hook (haskell-mode . dante-mode)
  )

(provide 'haskell-setup)
