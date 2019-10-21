;;; scala-setup.el --- summary -*- lexical-binding: t -*-

;; Author: adam bob
;; Maintainer: adam bob
;; Version: 1
;; Package-Requires: (dependencies)


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Scala configurtaion

;;; Code:

(use-package sbt-mode
  :demand t
  :commands sbt-start sbt-command
  :hook (sbt-mode . (lambda () (setenv "AWS_PROFILE" "scala-deps")))
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map))

(use-package scala-mode
  :demand t
  :mode "\\.s\\(cala\\|bt\\)$"
  :init
  (setq lsp-scala-server-command "/usr/local/bin/metals-emacs")
  (defun sbt-compile ()
    (interactive)
    (sbt-command "compile"))
  :bind
  (:map scala-mode-map
        ("C-c C-c C-c" . sbt-command)
        ("C-c C-c C-b" . sbt-compile)
        ("C-c C-c C-s". sbt-switch-to-active-sbt-buffer))
  :config
  (setq scala-indent:align-forms t
        scala-indent:align-parameters t
        scala-indent:indent-value-expression t
        scala-indent:default-run-on-strategy
        scala-indent:operator-strategy)
  :hook
  (scala-mode . smartparens-mode)
  (scala-mode . lsp)
  ;; (scala-mode . lsp-mode)
  (scala-mode . hs-minor-mode)
  (scala-mode . (lambda () (yas-load-directory (concat user-emacs-directory "snippets/scala-mode/")))))

(use-package ammonite-term-repl
  :hook (term-mode . smartparens-mode)
  :demand t
  :ensure t)

(provide 'scala-setup)

;;; scala-setup.el ends here
