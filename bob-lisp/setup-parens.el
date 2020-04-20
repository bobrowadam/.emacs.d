(use-package paredit
  :demand t
  :hook
  (emacs-lisp-mode . (lambda () (progn
                                  (smartparens-mode -1)
                                  (paredit-mode))))
  (eval-expression-minibuffer-setup . (paredit-mode))
  (racket-mode . paredit-mode))

(use-package smartparens
  :demand t
  :config
  (setq sp-ignore-modes-list '(minibuffer-inactive-mode emacs-lisp-mode eval-expression-minibuffer-setup))
  (show-smartparens-global-mode t)
  (smartparens-global-mode t)
  (sp-local-pair 'js2-mode "{ " " }" :trigger-wrap "{")
  :hook
  (comint-mode . smartparens-mode)
  (eshell-mode . smartparens-mode))

(provide 'setup-parens)
