;;; package --- Summary
;;;; Ido setup
;;; Commentary:
;;;
;;; Code:

(use-package ido
  :ensure t

  :init
  (setq ido-enable-prefix nil
        ido-enable-flex-matching t
        ido-case-fold nil
        ido-auto-merge-work-directories-length -1
        ido-create-new-buffer 'always
        ido-use-filename-at-point nil
        ido-max-prospects 10)

  ;; disable ido faces to see flx highlights.
  (setq ido-use-faces nil)

  (defun my/ido-go-straight-home ()
    (interactive)
    (cond
     ((looking-back "~/") (insert "projects/"))
     ((looking-back "/") (insert "~/"))
     (:else (call-interactively 'self-insert-command))))

  (defun my/setup-ido ()
    ;; Go straight home
    (define-key ido-file-completion-map (kbd "~") 'my/ido-go-straight-home)
    (define-key ido-file-completion-map (kbd "C-~") 'my/ido-go-straight-home)

    ;; Use C-w to go back up a dir to better match normal usage of C-w
    ;; - insert current file name with C-x C-w instead.
    (define-key ido-file-completion-map (kbd "C-w") 'ido-delete-backward-updir)
    (define-key ido-file-completion-map (kbd "C-x C-w") 'ido-copy-current-file-name)

    (define-key ido-file-dir-completion-map (kbd "C-w") 'ido-delete-backward-updir)
    (define-key ido-file-dir-completion-map (kbd "C-x C-w") 'ido-copy-current-file-name))
  
  :config
  (ido-mode t)
  ;;Better flex matching between words
  (use-package flx-ido
    :ensure t
    :config
    (flx-ido-mode 1))

  ;; flx-ido looks better with ido-vertical-mode
  (use-package  ido-vertical-mode
    :ensure t
    :init
    ;; C-n/p is more intuitive in vertical layout
    (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)
    :config
    (ido-vertical-mode))
  (add-hook 'ido-setup-hook 'my/setup-ido)
  ;; Always rescan buffer for imenu
  (set-default 'imenu-auto-rescan t)
  (add-to-list 'ido-ignore-directories "target")
  (add-to-list 'ido-ignore-directories "node_modules")

  ;; Ido at point (C-,)
  (use-package ido-at-point
    :ensure t
    :config (ido-at-point-mode))

  ;; Use ido everywhere
  (use-package ido-completing-read+
    :ensure t
    :config
    (ido-ubiquitous-mode 1)))

(provide 'setup-ido)
;;; setup-ido.el ends here
