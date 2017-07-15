;; Tramp
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


;;;; to connect via shadow:
;;;; /sshx:shadow|ssh:ubuntu@od-orenhazan|sudo:root@od-orenhazan:/
(add-to-list 'tramp-default-proxies-alist
             '("od-orenhazan" "\\`ubuntu\\'" "/sshx:shadow:")
             '("prod-einstein-1" "\\`ubuntu\\'" "/sshx:shadow:"))
(add-to-list 'tramp-restricted-shell-hosts-alist
             "\\shadow\\'")

;;Disable projectile mode line project naming for better performance:
(add-hook 'find-file-hook
          (lambda ()
            (when (file-remote-p default-directory)
              (setq-local projectile-mode-line "Projectile"))))

(provide 'setup-tramp)
