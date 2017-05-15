;;; package --- Summary
;;; Commentary:
;;; Generates a random element from a given list


;;; Code:
(setq rand-theme-wanted [ cyberpunk ])

(defun load-random-theme()
  (interactive)
  (load-theme (aref rand-theme-wanted (random (length rand-theme-wanted))) t nil ))

(provide 'load-random-theme)
;;; load-random-theme.el ends here
