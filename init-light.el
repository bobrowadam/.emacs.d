;; delete char and delte word with "C-h" "C-M-h"
(define-key key-translation-map [?\C-h] [?\C-?])
(define-key key-translation-map (kbd "<f1>") (kbd "C-h"))
;; (define-key key-translation-map (kbd "<f9>") (kbd "C-s-h"))
(global-set-key (kbd "M-C-h") 'backward-kill-word)
(require 'tramp)
(custom-set-variables
   '(tramp-password-prompt-regexp
     (concat
      "^.*"
      (regexp-opt
       '("passphrase" "Passphrase"
         ;; English
         "password" "Verification code"
         ;; Deutsch
         "passwort" "Passwort"
         ;; Français
         "mot de passe" "Mot de passe")
       t)
      ".*:\0? *")
     nil (tramp)))
(add-to-list 'tramp-restricted-shell-hosts-alist
               "\\shadow\\'")
(setq tramp-default-method "ssh")
