;; Don't load packages after init file loading
(setq package-enable-at-startup nil)
(add-to-list 'load-path "~/.emacs.d/setup-files/")
(add-to-list 'load-path "~/.emacs.d/elpa/bind-key-20180513.430/")
(add-to-list 'load-path "~/.emacs.d/elpa/use-package-20181119.2350/")
;; (load "~/.emacs.d/setup-files/sane-defaults.el")

;; (load "~/.emacs.d/elpa/use-package-20181119.2350/use-package")
(require 'use-package)
(require 'sane-defaults)
(use-package scala-mode
  :if (window-system)
  :load-path "~/.emacs.d/elpa/scala-mode-20170802.1132"
  :defer
  :mode "\\.s\\(cala\\|bt\\)$"
  :hook (scala-mode . highlight-indent-guides-mode)
  :config
  (setq scala-indent:align-forms t
        scala-indent:align-parameters t
        scala-indent:indent-value-expression t
        scala-indent:default-run-on-strategy
        scala-indent:operator-strategy))
