(setq custom-safe-themes t)
(setq custom-theme-directory (concat user-emacs-directory "themes"))
;; (set-frame-font "DaddyTimeMono Nerd Font 19")
;; (add-to-list 'default-frame-alist
;;              '(font . "DaddyTimeMono Nerd Font 19"))
(set-frame-font "FiraCode Nerd Font 19")
(add-to-list 'default-frame-alist
             '(font . "FiraCode Nerd Font 19"))
(use-package material-theme
  :disabled t
  :config (load-theme 'material :no-confirm))

(use-package modus-themes
  :config
  (setq modus-vivendi-palette-overridesd
      '((fg-main "#333333")
        (comment red-faint)
        (keyword cyan-cooler))))

(use-package ef-themes
  :demand t
  :config
  (ef-themes-select 'ef-elea-dark))

(use-package highlight-indent-guides
  :config
  (setq highlight-indent-guides-method 'fill
        highlight-indent-guides-responsive 'top
        highlight-indent-guides-auto-enabled t)
  :hook (prog-mode . highlight-indent-guides-mode))

(use-package moody
  :demand t
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode)
  (moody-replace-eldoc-minibuffer-message-function))

(use-package minions
  :demand t
  :config
  (minions-mode 1))

(use-package all-the-icons
  :disabled t
  :if (display-graphic-p))

(use-package nerd-icons
  :demand t)

(use-package nerd-icons-dired
  :after nerd-icons
  :hook
  (dired-mode . nerd-icons-dired-mode))

(provide 'appearance)
