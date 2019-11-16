(use-package yasnippet-snippets)
(use-package yasnippet
  :defer
  :hook
  (prog-mode-hook . yas-minor-mode)
  (emacs-lisp-mode . yas-minor-mode)
  :config
  (setq yas-snippet-dirs
        `(,(concat user-emacs-directory "snippets")
          ,yasnippet-snippets-dir))
  (yas-reload-all))

(provide 'setup-snippets)
