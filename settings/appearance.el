;;; package --- Summary
;;;; Emacs appearance
;;; Commentary:
;;;; Here we define how stuff look
;;; Code:

;; Show time:
(display-time)
(setq display-time-day-and-date 't)
(setq display-time-default-load-average nil)
(setq display-time-24hr-format 't)


;;;; Mode line:
(use-package rich-minority
  :ensure t
  :init
  (progn
    (setq rm-whitelist (setq rm-whitelist (mapconcat #'identity '( " Paredit" " Smartparens") "\\|"))))
  :config
  (rich-minority-mode 1 ))

(setq sml/theme 'automatic)
(setq sml/theme 'automatic)
(setq sml/theme 'dark)

;;;; loading:
(setq themes-wanted '(abyss lush manoj-dark cyberpunk purple-haze ample tronesque plan9 railscasts-reloaded planet zweilight))

(defun load-random-theme-with-sml ()
  "Call load-random-theme with sml/setup."
  (interactive)
  (load-random-favorite-theme themes-wanted '(sml/setup))
  (show-current-theme))

(load-random-theme-with-sml)
(global-set-key (kbd "C-c l r") 'load-random-theme-with-sml)

;; Font:
;; (set-face-attribute 'default nil :font "Hack 18")
(set-face-attribute 'default nil :font "Monaco 18")

(provide 'appearance)
;;; appearance.el ends here
