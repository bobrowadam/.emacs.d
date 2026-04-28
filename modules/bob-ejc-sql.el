;;; bob-ejc-sql.el --- ejc-sql JDBC client config for Bradwell -*- lexical-binding: t; -*-

;; Emacs SQL client backed by Clojure/JDBC.  Async queries, autocomplete
;; of table/column names pulled live from the DB, ElDoc for tables,
;; pretty result buffers.  Requires `leiningen' on $PATH and Java
;; (already installed).
;;
;; Install once:
;;   brew install leiningen
;;   drop the Postgres JDBC driver at ~/.emacs.d/jdbc/postgresql.jar
;;
;; Optional connection passwords can be read from ~/.authinfo.gpg:
;;
;;   machine bradwell-dev   login bradwell password <pw> port 5432
;;   machine bradwell-prod  login bradwell password <pw> port 5432
;;
;; Local is hardcoded as postgres/postgres.  Remote dev/prod connections
;; also support an authinfo-free flow:
;; `bob/ejc-connect-dev' and `bob/ejc-connect-prod' ensure AWS SSO is
;; logged in, fetch `/bradwell/ENV/db-password' from SSM, start
;; `task db-connect ENV=ENV', and then connect via localhost:5432.
;;
;;; Code:

(use-package clomacs)

(use-package ejc-sql
  :after clomacs
  :commands (ejc-connect ejc-connect-existing-repl bob/ejc-open)
  :custom
  ;; Use a non-default port so it doesn't fight with langfuse/skaffold on 8080.
  (clomacs-httpd-default-port 8090)
  (ejc-result-table-impl 'ejc-result-mode)
  (ejc-use-flx t)
  (ejc-flx-threshold 2)
  :config
  (require 'seq)
  (require 'subr-x)

  ;; --- Secrets from authinfo -------------------------------------------
  (defun bob/ejc--authinfo-entry (host)
    "Return the first auth-source entry for HOST, or nil."
    (require 'auth-source)
    (car (auth-source-search :host host :max 1)))

  (defun bob/ejc--authinfo-password (entry)
    "Return the plaintext password from an auth-source ENTRY."
    (let ((s (and entry (plist-get entry :secret))))
      (if (functionp s) (funcall s) s)))

  ;; --- Connection registry --------------------------------------------
  ;; Each connection is a plist: :name :host :db :user and optional
  ;; :password.  At load time, hardcoded passwords are registered first,
  ;; then ~/.authinfo.gpg entries are used as an optional override/fallback.
  ;; For remote dev/prod, `bob/ejc-connect-env' can also fetch the
  ;; password from SSM after ensuring AWS SSO is logged in.  Defaults
  ;; match the local minikube pod and the RDS schema used by services/
  ;; migrations.  Override user/db/password per-env by editing this list,
  ;; or user/password by editing the authinfo line (auth-source reads
  ;; :user too).
  (defvar bob/ejc-connections
    '((:name "bradwell-local" :host "bradwell-local" :db "bradwell" :user "postgres" :password "postgres")
      (:name "bradwell-dev"   :host "bradwell-dev"   :db "bradwell" :user "bradwell")
      (:name "bradwell-prod"  :host "bradwell-prod"  :db "bradwell" :user "bradwell"))
    "Bradwell ejc-sql connections.
Entries with :password are auto-registered.  Other entries are
registered iff a matching machine line exists in ~/.authinfo.gpg,
or when `bob/ejc-connect-env' fetches credentials from SSM.")

  (defun bob/ejc--create-connection (spec user password)
    "Register SPEC with ejc-sql using USER and PASSWORD."
    (let ((name (plist-get spec :name))
          (db   (plist-get spec :db)))
      (ejc-create-connection
       name
       :dependencies '[[org.postgresql/postgresql "42.7.4"]]
       :classname "org.postgresql.Driver"
       :subprotocol "postgresql"
       ;; All remote connections are expected to be tunneled to localhost:5432
       ;; (local minikube port-forward; `task db-connect ENV=<env>` for RDS).
       :subname (format "//localhost:5432/%s" db)
       :user user
       :password password)))

  (defun bob/ejc--register (spec)
    "Register the connection described by plist SPEC if a password is available."
    (let* ((host (plist-get spec :host))
           (default-user (plist-get spec :user))
           (default-password (plist-get spec :password))
           (entry (bob/ejc--authinfo-entry host))
           (pw    (or (bob/ejc--authinfo-password entry) default-password))
           (user  (or (and entry (plist-get entry :user)) default-user)))
      (when pw
        (bob/ejc--create-connection spec user pw))))

  (defun bob/ejc-registered-connections ()
    "Names of connections currently registered with ejc-sql."
    (when (boundp 'ejc-connections)
      (mapcar #'car ejc-connections)))

  (defun bob/ejc-refresh-connections ()
    "Register hardcoded connections and any bradwell-* entries in ~/.authinfo.gpg."
    (interactive)
    (auth-source-forget-all-cached)
    (mapc #'bob/ejc--register bob/ejc-connections)
    (message "ejc-sql: %s" (bob/ejc-registered-connections)))

  (bob/ejc-refresh-connections)

  ;; --- Corfu/CAPF bridge + ElDoc ----------------------------------------
  (defun bob/ejc--capf-candidates ()
    "Return merged ejc-sql candidates for completion at point."
    (delete-dups
     (append (or (ejc-get-ansi-sql-words) '())
             (or (ejc-get-keywords) '())
             (or (ejc-owners-candidates) '())
             (or (ejc-tables-candidates) '())
             (or (ejc-views-candidates) '())
             (or (ejc-packages-candidates) '())
             (or (ejc-colomns-candidates) '()))))

  (defun bob/ejc-capf ()
    "CAPF adapter for ejc-sql so Corfu can complete DB objects/keywords."
    (when (and (bound-and-true-p ejc-sql-mode)
               (ejc-buffer-connected-p))
      (let* ((dot-pos (save-excursion
                        (and (> (point) (point-min))
                             (eq (char-before) ?.)
                             (point))))
             (bounds (or (and dot-pos (cons dot-pos dot-pos))
                         (bounds-of-thing-at-point 'symbol)
                         (cons (point) (point))))
             (start (car bounds))
             (end (cdr bounds)))
        (list start end
              (completion-table-dynamic
               (lambda (_prefix) (bob/ejc--capf-candidates)))
              :exclusive 'no
              :annotation-function
              (lambda (cand)
                (cond
                 ((member cand (or (ejc-colomns-candidates) '())) " column")
                 ((member cand (or (ejc-tables-candidates) '())) " table")
                 ((member cand (or (ejc-views-candidates) '())) " view")
                 (t "")))))))

  (defun bob/ejc-complete-auto-complete-capf (&rest _)
    "When ejc async metadata arrives, retrigger CAPF for Corfu users."
    (when (and (bound-and-true-p corfu-mode)
               (bound-and-true-p ejc-sql-mode)
               (eq (current-buffer) (window-buffer (selected-window))))
      (completion-at-point)))

  (advice-add 'ejc-complete-auto-complete :after #'bob/ejc-complete-auto-complete-capf)

  (defun bob/ejc-enable-sql-mode ()
    "Enable `ejc-sql-mode' in SQL buffers."
    (unless (bound-and-true-p ejc-sql-mode)
      (ejc-sql-mode 1)))

  (add-hook 'sql-mode-hook #'bob/ejc-enable-sql-mode)

  (add-hook 'ejc-sql-minor-mode-hook
            (lambda ()
              (add-hook 'completion-at-point-functions #'bob/ejc-capf nil t)
              ;; `yas-minor-mode' usually shadows TAB in SQL buffers.
              (when (bound-and-true-p yas-minor-mode)
                (yas-minor-mode -1))
              (local-set-key (kbd "TAB") #'completion-at-point)
              (local-set-key (kbd "<tab>") #'completion-at-point)
              (ejc-eldoc-setup)))

  ;; --- Dispatcher used by scripts/.bob-ejc.sh and .dir-locals.el -------
  (defun bob/ejc-open (&optional connection)
    "Open or switch to an ejc-sql buffer for CONNECTION.
Prompts with the list of currently registered connections; defaults to
`bradwell-local' when available."
    (interactive
     (let* ((choices (or (bob/ejc-registered-connections)
                         (error "No ejc-sql connections registered. Reload bob-ejc-sql or run M-x bob/ejc-refresh-connections")))
            (default (if (member "bradwell-local" choices)
                         "bradwell-local"
                       (car choices))))
       (list (completing-read (format "ejc connection (default %s): " default)
                              choices nil t nil nil default))))
    (let* ((name (or connection "bradwell-local"))
           (bufname (format "*ejc: %s*" name))
           (buf (get-buffer-create bufname)))
      (unless (member name (bob/ejc-registered-connections))
        (user-error "EJC connection %s is not registered; run bob/ejc-connect-dev/prod or bob/ejc-refresh-connections" name))
      (with-current-buffer buf
        (unless (derived-mode-p 'sql-mode) (sql-mode))
        (unless (bound-and-true-p ejc-sql-mode) (ejc-sql-mode 1))
        (ejc-connect name))
      (pop-to-buffer buf)))

  ;; --- Tunnel + connect helpers ----------------------------------------
  (defcustom bob/bradwell-monorepo-root "~/source/bradwell-monorepo"
    "Path to the bradwell-monorepo checkout used for task commands."
    :type 'directory)

  (defun bob/ejc--wait-for-localhost-5432 (on-ready)
    "Poll localhost:5432 and call ON-READY once reachable."
    (let ((tries 0)
          timer)
      (setq timer
            (run-at-time
             0.5 0.5
             (lambda ()
               (setq tries (1+ tries))
               (if (ignore-errors
                     (let ((p (open-network-stream "ejc-port-check" nil "localhost" 5432)))
                       (delete-process p)
                       t))
                   (progn
                     (cancel-timer timer)
                     (funcall on-ready))
                 (when (> tries 120)
                   (cancel-timer timer)
                   (message "Timed out waiting for localhost:5432"))))))))

  (defun bob/ejc--connection-spec (name)
    "Return the connection spec named NAME."
    (seq-find (lambda (spec) (equal (plist-get spec :name) name))
              bob/ejc-connections))

  (defun bob/ejc--aws-profile (env)
    "Return the AWS profile for ENV."
    (format "bradwell-%s" env))

  (defun bob/ejc--aws-sso-authenticated-p (profile)
    "Return non-nil when PROFILE has usable AWS credentials."
    (eq 0 (ignore-errors
            (call-process "aws" nil nil nil
                          "sts" "get-caller-identity"
                          "--profile" profile))))

  (defun bob/ejc--ensure-aws-sso (env on-ready)
    "Ensure AWS SSO credentials exist for ENV, then call ON-READY."
    (let ((profile (bob/ejc--aws-profile env)))
      (if (bob/ejc--aws-sso-authenticated-p profile)
          (funcall on-ready)
        (let ((buf (get-buffer-create (format "*aws-sso:%s*" env))))
          (with-current-buffer buf (erase-buffer))
          (message "AWS SSO login required for %s; starting browser login (see %s)"
                   profile (buffer-name buf))
          (make-process
           :name (format "aws-sso-%s" env)
           :buffer buf
           :command (list "aws" "sso" "login" "--profile" profile)
           :noquery t
           :connection-type 'pipe
           :sentinel
           (lambda (proc _event)
             (when (eq (process-status proc) 'exit)
               (if (zerop (process-exit-status proc))
                   (progn
                     (message "AWS SSO login completed for %s" profile)
                     (funcall on-ready))
                 (message "AWS SSO login failed for %s (see %s)"
                          profile (buffer-name (process-buffer proc)))))))))))

  (defun bob/ejc--aws-output (&rest args)
    "Run aws ARGS and return trimmed stdout, or signal an error."
    (with-temp-buffer
      (let ((status (apply #'call-process "aws" nil t nil args))
            (command (concat "aws " (string-join args " "))))
        (unless (and (integerp status) (zerop status))
          (error "%s failed: %s" command (string-trim (buffer-string))))
        (string-trim (buffer-string)))))

  (defun bob/ejc--ssm-parameter (env path)
    "Return decrypted SSM parameter PATH for ENV."
    (bob/ejc--aws-output
     "ssm" "get-parameter"
     "--name" path
     "--with-decryption"
     "--query" "Parameter.Value"
     "--output" "text"
     "--profile" (bob/ejc--aws-profile env)))

  (defun bob/ejc--register-env-from-ssm (env)
    "Register bradwell ENV connection by fetching its password from SSM."
    (let* ((conn (format "bradwell-%s" env))
           (spec (or (bob/ejc--connection-spec conn)
                     (user-error "No ejc connection spec for %s" conn)))
           (password (bob/ejc--ssm-parameter env (format "/bradwell/%s/db-password" env)))
           (user (or (ignore-errors
                       (bob/ejc--ssm-parameter env (format "/bradwell/%s/rds-username" env)))
                     (plist-get spec :user))))
      (bob/ejc--create-connection spec user password)))

  (defun bob/ejc--start-db-connect (env conn buf directory)
    "Start the ENV database tunnel in BUF from DIRECTORY, then open CONN."
    (with-current-buffer buf (erase-buffer))
    (let ((default-directory directory))
      (make-process
       :name (format "db-connect-%s" env)
       :buffer buf
       :command (list "task" "db-connect" (format "ENV=%s" env))
       :noquery t
       :connection-type 'pipe))
    (message "Started task db-connect ENV=%s (see %s)" env (buffer-name buf))
    (bob/ejc--wait-for-localhost-5432
     (lambda ()
       (bob/ejc-refresh-connections)
       (unless (member conn (bob/ejc-registered-connections))
         (bob/ejc--register-env-from-ssm env))
       (bob/ejc-open conn))))

  (defun bob/ejc-connect-env (env)
    "Ensure AWS SSO, run `task db-connect ENV=ENV', then open ejc buffer."
    (interactive "sENV (dev/prod): ")
    (let* ((env (downcase env))
           (conn (format "bradwell-%s" env))
           (root (file-name-as-directory
                  (expand-file-name bob/bradwell-monorepo-root)))
           (buf (get-buffer-create (format "*db-connect:%s*" env))))
      (unless (member env '("dev" "prod"))
        (user-error "ENV must be dev or prod"))
      (bob/ejc--ensure-aws-sso
       env
       (lambda ()
         (condition-case err
             (progn
               (bob/ejc-refresh-connections)
               (unless (member conn (bob/ejc-registered-connections))
                 (bob/ejc--register-env-from-ssm env))
               (bob/ejc--start-db-connect env conn buf root))
           (error
            (message "Could not prepare %s ejc connection: %s"
                     conn (error-message-string err))))))))

  (defun bob/ejc-connect-dev ()
    "Start dev DB tunnel and open ejc connection."
    (interactive)
    (bob/ejc-connect-env "dev"))

  (defun bob/ejc-connect-prod ()
    "Start prod DB tunnel and open ejc connection."
    (interactive)
    (bob/ejc-connect-env "prod"))

  ;; Back-compat alias.
  (defalias 'bob/ejc-bradwell-local
    (lambda () (interactive) (bob/ejc-open "bradwell-local"))))

(provide 'bob-ejc-sql)
;;; bob-ejc-sql.el ends here
