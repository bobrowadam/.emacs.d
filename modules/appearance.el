(setq custom-safe-themes t)
(setq custom-theme-directory (concat user-emacs-directory "themes"))
(set-frame-font "DaddyTimeMono Nerd Font 21")
(add-to-list 'default-frame-alist
             '(font . "DaddyTimeMono Nerd Font 21"))

(use-package modus-themes
  :init
  (modus-themes-load-themes)
  :custom
  (modus-themes-vivendi-color-overrides '((bg-main . "#191a1b")))
  :config
  (modus-themes-load-vivendi))

(use-package ef-themes
  :disabled t
  :config
  (ef-themes-select 'ef-autumn))

(use-package highlight-indent-guides
  :config
  (setq highlight-indent-guides-method 'fill
        highlight-indent-guides-responsive 'top
        highlight-indent-guides-auto-enabled t)
  :hook (prog-mode . highlight-indent-guides-mode))

;; (use-package mood-line
;;   :demand t
;;   :config (mood-line-mode))

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
  :if (display-graphic-p))

(provide 'appearance)
