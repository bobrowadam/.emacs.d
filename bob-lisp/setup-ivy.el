(use-package smex)

(use-package amx
  :after ivy
  :disabled t
  :custom
  (amx-backend 'auto)
  (amx-save-file "~/.emacs.d/amx-items")
  (amx-history-length 50)
  (amx-show-key-bindings t)
  :config
  (amx-mode 1))

(use-package ivy
  :demand t
  :config
  (setq ivy-re-builders-alist
        '((t . ivy--regex-fuzzy)))
  (setq ivy-use-selectable-prompt t)
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d%d) ")
  (ivy-mode 1))

(use-package ivy-posframe
  :disabled t
  :demand t
  :config
  (setq ivy-posframe-display-functions-alist
      '((swiper          . ivy-posframe-display-at-point)
        (complete-symbol . ivy-posframe-display-at-point)
        (counsel-M-x     . ivy-posframe-display-at-window-bottom-left)
        (counsel-find-file . ivy-posframe-display-at-window-bottom-left)
        (ivy-switch-buffer . ivy-posframe-display-at-frame-center)
        (projectile-switch-to-buffer . ivy-posframe-display-at-frame-center)
        (t               . ivy-posframe-display-at-frame-center)))
  (setq ivy-posframe-parameters
      '((left-fringe . 8)
        (right-fringe . 8)))
  (ivy-posframe-mode 1))

(use-package counsel
  :if (window-system)
  ;; :after ivy
  ;; :init
  ;; (setq counsel-rg-base-command
  ;;       "rg --heading --context-separator \" \" -i -M 120 --line-number --color never %s .")
  ;; :config
  ;; (setcdr (assoc 'counsel-M-x ivy-initial-inputs-alist) "")
  )

(provide 'setup-ivy)
