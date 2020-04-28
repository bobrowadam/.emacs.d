(use-package tramp
  :config
  (setq tramp-password-prompt-regexp
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
         ".*:\0? *"))
  (setq tramp-default-method "ssh")
  (add-to-list 'tramp-restricted-shell-hosts-alist
               "\\bastion\\'")
  (add-to-list 'tramp-restricted-shell-hosts-alist
               "\\bastion_local\\'")
  
  (add-to-list 'tramp-default-proxies-alist
               '("bob" nil "/sshx:bastion:")))

(provide 'tramp-settings)
