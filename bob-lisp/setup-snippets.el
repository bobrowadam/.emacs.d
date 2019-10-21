(use-package yasnippet
  :demand
  :hook
  (prog-mode-hook . yas-minor-mode)
  (emacs-lisp-mode . yas-minor-mode)
  :config
  (setq yas-snippet-dirs
        `(,(concat user-emacs-directory "snippets")
          ,(concat user-emacs-directory "elpa/yasnippet-snippets-20190926.1252/snippets")))
  :config (yas-reload-all))
(use-package yasnippet-snippets)

(provide 'setup-snippets)
