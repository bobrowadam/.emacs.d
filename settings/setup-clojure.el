(use-package clojure-mode
  :ensure t)

(use-package cider
  :ensure t)

(use-package dash-at-point
  :ensure t
  :init
  (defun clojure-set-local-dash ()
    (setq-local dash-at-point-docset "Clojure"))
  :hook
  (clojure-mode-hook . clojure-set-local-dash)
  :config
  (add-to-list 'dash-at-point-mode-alist '(clojure-mode . "Clojure")))

(provide 'setup-clojure)
