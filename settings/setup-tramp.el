;;; Package --- Summary:
;;; Commentary:
;; Tramp setup

;;; Code:
(use-package tramp
  :init
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
  :config
  ;; to connect via proxy:
  ;; /sshx:<proxy-server-name>|ssh:ubuntu@<server name>|sudo:root@<server-name>:/
  (add-to-list 'tramp-restricted-shell-hosts-alist
               "\\shadow\\'")

  ;;Disable projectile mode line project naming for better performance:
  (add-hook 'find-file-hook
            (lambda ()
              (when (file-remote-p default-directory)
                (setq-local projectile-mode-line "Projectile")))))

(defun my-find-over-proxy (proxy server-name)
  "Find file through PROXY on SERVER-NAME."
  (interactive "sEnter proxy:
sEnter server name: ")
  (find-file (format "/sshx:%s|ssh:ubuntu@%s|sudo:root@%s:/" proxy server-name server-name)))

(defun mongo-over-proxy (proxy server-name)
  "Open inf-mongo on through PROXY on  SERVER-NAME."
  (interactive "sEnter proxy:\s
sEnter server name: ")
  (pop-to-buffer "*mongo*")
  (setq default-directory (format "/sshx:%s|ssh:ubuntu@%s:/" proxy server-name))
  (inf-mongo "/usr/bin/mongo 127.0.0.1:27017"))

(global-set-key (kbd "C-c C-x C-f") 'my-find-over-proxy)

(provide 'setup-tramp)
;;; setup-tramp.el ends here
