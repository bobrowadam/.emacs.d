;;; package --- Summary
;;; Commentary:
;;; Generates a random element from a given list


;;; Code:

(defun load-random-favorite-theme(themes-wanted &optional function)
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme (nth (random (length themes-wanted)) themes-wanted) t nil )
  (when function
    (eval function)))

(defun load-next-favorite-theme (themes-wanted &optional function)
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme (cons #'nthcdr custom-enabled-themes )))

(provide 'load-random-theme)
;;; load-random-theme.el ends here
