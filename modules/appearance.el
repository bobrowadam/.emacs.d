(setq custom-safe-themes t)
(setq custom-theme-directory (concat user-emacs-directory "themes"))
;; (set-frame-font "DaddyTimeMono Nerd Font 19")
;; (add-to-list 'default-frame-alist
;;              '(font . "DaddyTimeMono Nerd Font 19"))
(set-frame-font "FiraCode Nerd Font 19")
(add-to-list 'default-frame-alist
             '(font . "FiraCode Nerd Font 19"))

(defun remote-config-p ()
  (and (boundp 'remote-mode) remote-mode))

(use-package modus-themes
  :demand t
  :config
  (setq modus-vivendi-palette-overridesd
          '((fg-main "#333333")
            (comment red-faint)
            (keyword cyan-cooler)))
    (if (remote-config-p)
        (modus-themes-select 'modus-operandi-tinted)
      (modus-themes-select 'modus-vivendi-tinted)))

(use-package spacious-padding
  :unless (remote-config-p)
  :demand t
  :config (spacious-padding-mode 1))

(use-package ef-themes
  :unless (remote-config-p)
  :disabled t
  :config
  (ef-themes-select 'ef-elea-dark))

(use-package highlight-indent-guides
  :config
  (setq highlight-indent-guides-method 'fill
        highlight-indent-guides-responsive 'top
        highlight-indent-guides-auto-enabled t)
  :hook (prog-mode . highlight-indent-guides-mode))

(use-package all-the-icons :demand t
  :if (display-graphic-p))

(use-package bobs-modeline
  :after modus-themes
  :unless (remote-config-p)
  :ensure nil
  :demand t
  :load-path "~/source/bobs-modeline/")

(provide 'appearance)
