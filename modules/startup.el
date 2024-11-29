(use-package dash :demand t)
(use-package s :demand t)
(use-package cl-lib :demand t)
(use-package server :demand t)

(unless (server-running-p)
  (server-start))

(use-package bobs-utils
  :demand t
  :load-path "./site-lisp"
  :ensure nil)

(defvar bootstrap-version)
(defun setup-straight ()
  (let ((bootstrap-file
         (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
        (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))
  (straight-use-package 'el-patch))

(time (setup-straight))
(setq service-directory (concat (getenv "HOME") "/source/services"))

(use-package exec-path-from-shell
  :if (window-system)
  :config
  (setq exec-path-from-shell-arguments nil))

(use-package fnm
  :if (window-system)
  :demand t
  :straight (:host github :repo "bobrowadam/fnm.el")
  :ensure nil)

(use-package short-lambda
  :if (window-system)
  :load-path "./site-lisp"
  :demand t
  :init
  (defun insert-λ ()
    (interactive)
    (insert "λ"))
  :bind
  ("C-x 8 l" . insert-λ))

(setq initial-major-mode 'fundamental-mode)

(use-package request)
(use-package plz)

(use-package edit-funcs
  :if (window-system)
  :load-path "./site-lisp"
  :bind
  ("C-`" . unpop-to-mark-command)
  ("M-`" . jump-to-mark))

(use-package scratch-pop
  :bind ("C-c r" . scratch-pop))

(provide 'startup)
