;;; package --- Summary
;;; Commentary:
;; Functions for remote actions
;;; Code:

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
  (find-file (format-default-dir server-name "ubuntu" proxy nil)))

(global-set-key (kbd "C-c C-x C-f") 'my-find-over-proxy)

(defun mongo-over-proxy (proxy server-name)
  "Open inf-mongo on through PROXY on  SERVER-NAME."
  (interactive "sEnter proxy:\s
sEnter server name: ")
  (pop-to-buffer "*mongo*")
  (setq default-directory (format "/sshx:%s|ssh:ubuntu@%s:/" proxy server-name))
  (inf-mongo "/usr/bin/mongo 127.0.0.1:27017"))

(provide 'remote-defuns)
;;; remote-defuns.el ends here
