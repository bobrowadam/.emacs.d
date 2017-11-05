;;; package --- summary
;;; Commentary:
;;; Functions for encrypting stuff with a node js script I wrote
;;; code:

(defun hash-region (p1 p2)
  "Hash string in selected region P1 - P2."

  (interactive "r")
  (let ((scriptName "~/.nvm/versions/node/v4.2.4/bin/node /Users/adam/source/node-scripts/hash.password.js"))
    (save-excursion
      (shell-command-on-region p1 p2 scriptName nil "REPLACE" nil t))))

(provide 'hash-region)
;;; hash-region.el ends here
