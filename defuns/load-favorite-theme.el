;;; package --- Summary
;;; Commentary:
;;; Generates a random element from a given list


;;; Code:

(defun load-random-favorite-theme(themes-wanted &optional function)
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme (nth (random (length themes-wanted)) themes-wanted) t nil )
  (when function
    (eval function)))

(provide 'load-random-theme)
;;; load-favorite-theme.el ends here
