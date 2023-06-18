(use-package dash :demand t)
(use-package s :demand t)
(use-package cl-lib :demand t)
(use-package server :demand t)
(use-package fnm
  :load-path "./site-lisp"
  :ensure nil
  :demand t)

(unless (server-running-p)
  (server-start))

(defvar bootstrap-version)
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
(straight-use-package 'el-patch)


(setq lsp-clients-typescript-npm-location
        (fnm-npm-path "18"))

(setq service-directory (concat (getenv "HOME") "/source/services"))

(use-package exec-path-from-shell
  :ensure t
  :if (window-system)
  :demand t
  :config
  (add-to-list 'exec-path-from-shell-variables "BOB_DIR")
  (add-to-list 'exec-path-from-shell-variables "WHATSAPP_NUMBER")
  (add-to-list 'exec-path-from-shell-variables "LOCAL_WHATSAPP_NUMBER")
  (setq exec-path-from-shell-arguments nil)  
  (exec-path-from-shell-initialize))

(use-package short-lambda
  :load-path "./site-lisp"
  :demand t
  :init
  (defun insert-λ ()
    (interactive)
    (insert "λ"))
  :bind
  ("C-x 8 l" . insert-λ))

(setq initial-major-mode 'lisp-interaction-mode)

(use-package request)
(use-package plz)

(use-package bobs-utils
  :demand t
  :load-path "./site-lisp"
  :ensure nil)

(use-package edit-funcs
  :if (window-system)
  :load-path "./site-lisp"
  :bind
  ("C-`" . unpop-to-mark-command)
  ("M-`" . jump-to-mark))

(use-package scratch-pop
  :bind ("C-c r" . scratch-pop))

(provide 'startup)
