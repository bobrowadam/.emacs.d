(setq custom-safe-themes t)
(setq custom-theme-directory (concat user-emacs-directory "themes"))
(set-frame-font "DaddyTimeMono Nerd Font 21")
(add-to-list 'default-frame-alist
             '(font . "DaddyTimeMono Nerd Font 21"))

(use-package modus-themes
  :demand t
  :init
  (modus-themes-load-themes)
  :custom
  (modus-themes-vivendi-color-overrides '((bg-main . "gray8")))
  (modus-themes-mode-line '(borderless padded accented))
  (modus-themes-region '( bg-only))
  (modus-themes-bold-constructs t)
  (modus-themes-italic-constructs t)
  (modus-themes-paren-math ('bold intense))
  (modus-themes-hl-line (quote (accented)))
  (modus-themes-syntax '(alt-syntax yellow-comments green-strings))
  :config
  (modus-themes-load-vivendi))

(use-package highlight-indent-guides
  :config
  (setq highlight-indent-guides-method 'fill
        highlight-indent-guides-responsive 'top
        highlight-indent-guides-auto-enabled t)
  :hook (prog-mode . highlight-indent-guides-mode))

(use-package mood-line
  :demand t
  :config (mood-line-mode))

(use-package all-the-icons
  :if (display-graphic-p))

(provide 'appearance)
