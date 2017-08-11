;; Yas-snippets:

(use-package yasnippet
  :ensure t
  :bind
  (("C-c y e" . yas-expand)
   ("C-c y i" . yas-insert-snippet)))

(provide 'setup-yas)
