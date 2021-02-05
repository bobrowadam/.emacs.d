(use-package js2-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  :hook
  ;; (js2-mode . setup-tide-mode)
  (js2-mode . lsp-deferred)
  (js2-mode . js2-imenu-extras-mode)
  (js2-mode . js2-mode-hide-warnings-and-errors)
  (js2-mode . electric-indent-mode)
  (js2-mode . yas-minor-mode)
  (js2-mode . add-node-modules-path)
  :bind (:map js2-mode-map
              ("C-<tab>" . js2-indent-bounce)
              ("C-c C-s" . nil)
              ("C-x C-e" . js-send-last-sexp))
  :config
  (setq-default js2-auto-indent-p nil)
  (setq-default unset-electric-indent)
  (setq-default js-indent-level 2))

(use-package nodejs-repl)
(use-package json-mode
  :hook (json-mode . origami-mode)
  :bind (:map json-mode-map ("C-=" . origami-toggle-node)))

(use-package jq-format
  :demand t
  :after json-mode)

;; ################# DISABLED #########################
(use-package js-comint :disabled t :after js2-mode)
(use-package ts-comint :disabled t)

(use-package indium
  :disabled t
  :hook
  (js2-mode . indium-interaction-mode)
  (js-mode . indium-interaction-mode)
  :bind
  (:map indium-debugger-mode-map ("C-c C-SPC" . indium-debugger-step-over)))

(provide 'setup-js)
