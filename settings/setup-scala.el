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
  ("scala" . scala-mode))

(provide 'setup-scala)
;;; key-bindings.el ends here
