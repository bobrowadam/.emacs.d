(require 'dash)
(require 's)
(require 'subr-x)
(require 'cl-lib)
(require 'server)

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

(use-package exec-path-from-shell
  :if (window-system)
  :init
  (setq exec-path-from-shell-arguments nil)
  :demand t
  :config
  (add-to-list 'exec-path-from-shell-variables "BOB_DIR")
  (add-to-list 'exec-path-from-shell-variables "WHATSAPP_NUMBER")
  (add-to-list 'exec-path-from-shell-variables "LOCAL_WHATSAPP_NUMBER")
  (exec-path-from-shell-initialize)
  (setq service-directory (concat (getenv "HOME") "/source/services")))

(use-package short-lambda
  :load-path "./site-lisp"
  :demand t
  :init
  (defun insert-λ ()
    (interactive)
    (insert "λ"))
  :bind
  ("C-x 8 l" . insert-λ))

(setq initial-major-mode 'fundamental-mode)

(provide 'startup)
