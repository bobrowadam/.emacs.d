;; Yas-snippets:

(use-package yasnippet
  :ensure t
  :bind
  (("C-c & C-e" . yas-expand)
   ("C-c & C-s" . yas-insert-snippet)
   ("C-c & C-n" . yas-new-snippet))
  :config
  (yas-global-mode 1))

(provide 'setup-yas)
