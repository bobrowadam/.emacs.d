;;;; package --- Summary
;;; Commentary:
;; Setup Scala mode
;;
;;; code:

(use-package ensime
  :ensure t
  :pin melpa-stable)

(use-package sbt-mode
  :ensure t
  :pin melpa-stable)

(use-package scala-mode
  :ensure t
  :pin melpa-stable
  :interpreter
  ("scala" . scala-mode)
  :init
  (add-hook 'scala-mode-hook '(lambda () (flyspell-prog-mode))))

(use-package dash-at-point
  :ensure t
  :config
  (add-hook 'scala-mode-hook (lambda ()
                               (setq dash-at-point-docset "Scala")))
  (add-to-list 'dash-at-point-mode-alist '(scala-mode . "Scala")))

(provide 'setup-scala)
;;; key-bindings.el ends here
