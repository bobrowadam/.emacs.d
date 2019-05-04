(use-package paredit
  :demand t
  :hook
  (emacs-lisp-mode . paredit-mode)
  (eval-expression-minibuffer-setup . paredit-mode))

(use-package smartparens
  :demand t
  :bind (:map smartparens-mode-map
              ("M-(" . sp-wrap-round)
              ("M-s" . sp-unwrap-sexp)
              ("C-)" . sp-forward-slurp-sexp)
              ("C-}" . sp-forward-barf-sexp)
              ("C-{" . sp-backward-barf-sexp)
              ("C-(" . sp-backward-slurp-sexp)
              ("C-'" . sp-rewrap-sexp)
              ("M-S" . sp-split-sexp)
              ("M-J" . sp-join-sexp)
              ("M-W" . sp-copy-sexp))
  :config
  (setq sp-ignore-modes-list '(minibuffer-inactive-mode emacs-lisp-mode eval-expression-minibuffer-setup))
  (show-smartparens-global-mode t)
  (smartparens-global-mode t)
  (sp-local-pair 'js2-mode "{ " " }" :trigger-wrap "{")
  :hook
  (comint-mode . smartparens-mode))

(provide 'setup-parens)
