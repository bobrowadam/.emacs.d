(setq custom-safe-themes t)
(setq custom-theme-directory (concat user-emacs-directory "themes"))
(set-frame-font "DaddyTimeMono Nerd Font 21")
(add-to-list 'default-frame-alist
             '(font . "DaddyTimeMono Nerd Font 21"))

(use-package modus-themes
  :demand t
  :custom
  (modus-themes-mode-line '(borderless padded accented))
  (modus-themes-region '( bg-only))
  (modus-themes-completions
   (quote ((t . (extrabold intense background)))))
  (modus-themes-bold-constructs t)
  (modus-themes-italic-constructs t)
  (modus-themes-paren-math ('bold intense))
  (modus-themes-hl-line nil)
  (modus-themes-syntax '(alt-syntax yellow-comments green-strings))
  :config
  (load-theme 'modus-vivendi))

(use-package mood-line
  :demand t
  :config (mood-line-mode))

(provide 'appearance)
