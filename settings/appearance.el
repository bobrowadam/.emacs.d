;;; package --- Summary
;;; Commentary:
;;; Here we define how stuff look
;;; Code:

;; Theme loading:
;; pick a random theme
(setq rand-theme-wanted [abyss lush manoj-dark cyberpunk purple-haze ample tronesque plan9 railscasts-reloaded planet zweilight])

(load-random-theme)

;;Mode line theme
;; you may setq sml/theme to one of the following: ('respectful 'light 'dark)
(setq sml/theme 'dark)
(sml/setup)

;; Font:
(set-face-attribute 'default nil :font "Monaco 18")

(provide 'appearance)
;;; appearance.el ends here
