
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)
(smartparens-global-mode t)
(define-key key-translation-map [?\C-h] [?\C-?])
(define-key key-translation-map (kbd "<f1>") (kbd "C-h"))
(global-set-key (kbd "M-C-h") 'backward-kill-word)
(setq user-init-file (or load-file-name (buffer-file-name)))
(setq user-emacs-directory (file-name-directory user-init-file))
(defalias 'yes-or-no-p 'y-or-n-p)

(setq display-time-day-and-date 't)
(setq display-time-default-load-average nil)
(setq display-time-24hr-format 't)
(display-time)

(tool-bar-mode -1)
(blink-cursor-mode -1)
(toggle-scroll-bar -1)
(delete-selection-mode 1)
(setq line-number-mode t)
(setq column-number-mode t)
(setq fill-column 80)
(set-default 'indent-tabs-mode nil)
(setq-default tab-width 2)
(set-default 'indicate-empty-lines t)
(load-theme 'tango-dark)
(ido-mode 't)
(whole-line-or-region-global-mode 1)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(setq shell-file-name "/bin/bash")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (whole-line-or-region))))
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
;; (add-to-list 'tramp-restricted-shell-hosts-alist
;;               "\\shadow\\'")
(setq tramp-default-method "ssh")

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(defun generate-clients-names (name number-range)
  "Generate an alist of clients name given a NAME and a NUMBER-RANGE."
  (let ((number-list (mapcar #'string-to-number (split-string number-range "-"))))
    (mapcar (lambda (num)
              ( concat name "-" (number-to-string num)))
            (number-sequence (car number-list)
                             (cadr number-list)))))

(defun remote-shell (server-name &optional user proxy sudo)
  "Open bash shell on remote SERVER-NAME.
You can also pass USER PROXY and SUDO as boolean."
  (let ((default-directory (format-default-dir server-name user proxy sudo))
        (shell-file-name "/bin/bash"))
    (shell (generate-buffer-name-for-server server-name))))

(defun remote-shell-command-to-string (command server-name &optional user proxy sudo)
  "Run shell COMMAND on remote SERVER-NAME.
You can also pass USER PROXY and SUDO as Boolean."
  (let ((default-directory (format-default-dir server-name user proxy sudo))
        (shell-file-name "/bin/bash"))
    (save-excursion)
    (set-buffer (get-buffer-create (generate-buffer-name-for-server server-name)))
    (insert (shell-command-to-string command))))

(defun remote-shell-command (command server-name &optional user proxy sudo)
  "Run shell COMMAND on remote SERVER-NAME.
You can also pass USER PROXY and SUDO as Boolean."
  (let ((default-directory (format-default-dir server-name user proxy sudo))
        (shell-file-name "/bin/bash"))
    (save-excursion)
    (set-buffer (get-buffer-create (generate-buffer-name-for-server server-name)))
    (shell-command command (generate-buffer-name-for-server server-name))))

(defun format-default-dir (server-name &optional user proxy sudo)
  "Format and return the default directory on a remote SERVER-NAME.
You can also pass USER PROXY and SUDO as Boolean."
  (if (or sudo proxy user)
      (cond ((and sudo proxy user)
             (format "/ssh:%s|ssh:%s@%s|sudo:root@%s:/" proxy user server-name server-name))
            ((and proxy user)
             (format "/ssh:%s|ssh:%s@%s:/" proxy user server-name))
            ((and sudo proxy)
             (format "/ssh:%s|ssh:%s|sudo:%s:/" proxy server-name server-name))
            ((and sudo user)
             (format "/ssh:%s|sudo:%s@%s:/" server-name user server-name))
            (sudo
             (format "/ssh:%s|sudo:%s:/" server-name server-name)))
    (format "/%s:/" server-name)))

(defun generate-buffer-name-for-server (server-name)
  "Return buffer name for remote shell on SERVER-NAME."
  (format "shell:%s" server-name))

(defun run-command-over-servers (sudo server-name command num-range &optional user proxy)
  (interactive "P
sEnter server name:\s
sEnter command:\s
sEnter num range:\s
sEnter user:\s
sEnter proxy:\s")
  (let ((api-clients (generate-clients-names server-name num-range)))
    (remote-shell-commands command api-clients user proxy sudo)
    (remote-shell-command-show api-clients)))

(defun remote-shell-commands (command servers-names user proxy sudo)
  "Run COMMAND over SERVERS-NAMES.
Can use USER, PROXY and SUDO as Boolean."
  (dolist (server-name servers-names)
    (remote-shell-command command server-name user proxy sudo)))

(defun remote-shell-command-show (clients)
  "Show the result of running `run-command-over-servers` over CLIENTS."
  (delete-other-windows)
  (let ((first-time t))
    (dolist (hostname clients)
      (if (not first-time)
          (split-window-vertically)
        (split-window-horizontally))
      (setq first-time nil)
      (other-window 1)
      (switch-to-buffer (generate-buffer-name-for-server hostname))
      (balance-windows)
      (sit-for 0.5))))


(defun my-find-over-proxy (proxy server-name)
  "Find file through PROXY on SERVER-NAME."
  (interactive "sEnter proxy:\s
sEnter server name: ")
  (find-file (format-default-dir server-name "ubuntu" proxy 't)))

(global-set-key (kbd "C-c C-x C-f") 'my-find-over-proxy)

(defun mongo-over-proxy (proxy server-name)
  "Open inf-mongo on through PROXY on  SERVER-NAME."
  (interactive "sEnter proxy:\s
sEnter server name: ")
  (pop-to-buffer "*mongo*")
  (setq default-directory (format "/sshx:%s|ssh:ubuntu@%s:/" proxy server-name))
  (inf-mongo "/usr/bin/mongo 127.0.0.1:27017"))
