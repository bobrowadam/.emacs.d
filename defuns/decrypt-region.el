;;; package --- summary
;;; Commentary:
;;; Functions for encrypting stuff with a node js script I wrote
;;; code:

(defun decrypt-regions-string (p1 p2)
 "Decrypt string in selected region P1 - P2.
The decryption is being made with a node script and the secret key is hard coded
so you should change it if needed."
  (interactive "r")
  (let ((scriptName " /Users/adambobrow/.nvm/versions/node/v4.2.4/bin/node /Users/adambobrow/source/crypto-utils/decryptor.js"))
    (save-excursion
      (shell-command-on-region p1 p2 scriptName nil "REPLACE" nil t))
    (json-mode)
    (json-mode-beautify))
  (json-mode))

(provide 'decrypt-region)
;;; decrypt-region.el ends here
