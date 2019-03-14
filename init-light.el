(package-initialize)
(setq linum-format "%2d  ")
(define-key key-translation-map [?\C-h] [?\C-?])
(define-key key-translation-map (kbd "<f1>") (kbd "C-h"))
(global-set-key (kbd "M-C-h") 'backward-kill-word)
(global-set-key  (kbd "C-x ;") 'comment-line)
(require 'tramp)
(global-set-key (kbd "M-SPC") 'mark-sexp)
;; (whole-line-or-region-global-mode)
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
(load-theme 'wombat)
