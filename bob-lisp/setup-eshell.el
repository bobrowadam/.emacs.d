(use-package eshell
  :demand t
  :init
  (setq eshell-hist-rebind-keys-alist
   (quote
    (([(control 112)]
      . eshell-previous-input)
     ([(control 110)]
      . eshell-next-input)
     ([(control up)]
      . eshell-previous-input)
     ([(control down)]
      . eshell-next-input)
     ([(control 114)]
      . eshell-isearch-backward)
     ([(control 115)]
      . eshell-isearch-forward)
     ([]
      . eshell-previous-matching-input)
     ([(meta 115)]
      . eshell-next-matching-input)
     ([(meta 112)]
      . eshell-previous-matching-input-from-input)
     ([(meta 110)]
      . eshell-next-matching-input-from-input)
     ([up]
      . eshell-previous-matching-input-from-input)
     ([down]
      . eshell-next-matching-input-from-input))))
  (defalias 'em 'find-file)
  (defalias 'status 'magit-status))

(use-package eshell-prompt-extras
  :disabled t
  :if (window-system)
  :config
  (autoload 'epe-theme-lambda "eshell-prompt-extras")
  (setq eshell-highlight-prompt nil
        eshell-prompt-function 'epe-theme-lambda))

(use-package eshell-git-prompt
  :demand t
  :ensure t
  :config
  (eshell-git-prompt-use-theme 'git-radar))

(provide 'setup-eshell)
