;; Yas-snippets:

(use-package yasnippet
  :ensure t
  :bind
  (("C-c & C-e" . yas-expand)
   ("C-c & C-s" . yas-insert-snippet)
   ("C-c & C-n" . yas-new-snippet))
  :config
  (yas-global-mode 1))

(use-package mocha-snippets
  :ensure t
  :config
  (setq mocha-snippets-use-fat-arrows t)
  (setq mocha-snippets-add-space-after-function-keyword t))


(provide 'setup-yas)
