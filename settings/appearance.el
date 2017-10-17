;;; package --- Summary
;;;; Emacs appearance
;;; Commentary:
;;;; Here we define how stuff look
;;; Code:

;; Show time:
(setq display-time-day-and-date 't)
(setq display-time-default-load-average nil)
(setq display-time-24hr-format 't)
(display-time)

;; Line numbers
;; highlight the current line number
(use-package hlinum
  :ensure t
  :config
  (hlinum-activate))

(setq linum-format " %3d ")
;; turn on line numbers in prog-mode
(add-hook 'prog-mode-hook 'linum-mode)

;; No scrollbar:
(toggle-scroll-bar -1)

;;;; Mode line show only whitelisted symbols and not the whole minor modes:
(use-package rich-minority
  :ensure t
  :init
  (setq rm-whitelist (setq rm-whitelist (mapconcat #'identity '( " Paredit" " Smartparens") "\\|")))
  :config
  (rich-minority-mode 1 ))

;;;; loading:
;; installing all themes:
(setq themes-to-install '(abyss-theme lush-theme cyberpunk-theme purple-haze-theme ample-theme tronesque-theme plan9-theme railscasts-reloaded-theme planet-theme zweilight-theme afternoon-theme))

(defun install-themes (themes-list)
  (let ( (theme (car themes-list))
         (next-themes (cdr themes-list)))
    (unless (package-installed-p theme)
      (package-install theme))
    (when next-themes (install-themes next-themes))))

(install-themes themes-to-install)

(use-package smart-mode-line
  :ensure t
  :init
  (defun show-current-theme()
    "show the current enabled theme"
    (interactive)
    (message "current enabled theme is %s" (cdr custom-enabled-themes)))
  
  :config
  (setq sml/theme 'dark)
  (setq sml/no-confirm-load-theme t)
  (load-theme 'lush)
  (sml/setup)
  (show-current-theme))

;; Random theme:

(setq themes-wanted '(abyss lush manoj-dark cyberpunk purple-haze ample tronesque plan9 railscasts-reloaded planet zweilight afternoon))

(defun load-random-favorite-theme(themes-wanted &optional function)
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme (nth (random (length themes-wanted)) themes-wanted) t nil )
  (when function
    (eval function)))

(defun load-random-theme-with-sml ()
  (interactive)
  (load-random-favorite-theme themes-wanted '(sml/setup)))

(global-set-key (kbd "C-c l r") 'load-random-theme-with-sml)

;; Fonts:
;; (set-face-attribute 'default nil :font "Hack 18")
;; (set-face-attribute 'default nil :font "Monaco 18")
(set-face-attribute 'default nil
                    :font "SauceCodePro Nerd Font 18"
                    :weight 'light)

(use-package rainbow-delimiters
  :ensure t
  :config (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(provide 'appearance)

;;; appearance.el ends here
