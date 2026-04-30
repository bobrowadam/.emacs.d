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
;; logged in, read the DB password from authinfo or SSM, start an
;; SSM tunnel, and then connect via a per-environment localhost port
;; (dev 15432, prod 25432).
;;
;;; Code:

(require 'ejc-sql nil t)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)

;; Use a non-default port so it doesn't fight with langfuse/skaffold on 8080.
(setq clomacs-httpd-default-port 8090
      ejc-result-table-impl 'ejc-result-mode
      ejc-use-flx t
      ejc-flx-threshold 2)

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
  ;; :password and :port.  At load time, hardcoded passwords are registered first,
  ;; then ~/.authinfo.gpg entries are used as an optional override/fallback.
  ;; For remote dev/prod, `bob/ejc-connect-env' can also fetch the
  ;; password from authinfo or SSM after ensuring AWS SSO is logged in.  Defaults
  ;; match the local minikube pod and the RDS schema used by services/
  ;; migrations.  Override user/db/password per-env by editing this list,
  ;; or user/password by editing the authinfo line (auth-source reads
  ;; :user too).
  (defvar bob/ejc-connections nil
    "Bradwell ejc-sql connections.
Entries with :password are auto-registered.  Other entries are
registered iff a matching machine line exists in ~/.authinfo.gpg,
or when `bob/ejc-connect-env' fetches credentials from SSM.")

  ;; Use `setq' so reloading this module updates an existing Emacs session.
  (setq bob/ejc-connections
        '((:name "bradwell-local" :host "bradwell-local" :db "bradwell" :user "postgres" :password "postgres" :port 5432)
          ;; Remote envs use the RDS master user `postgres` (matches
          ;; /bradwell/<env>/rds-username in SSM).  Authinfo can override.
          (:name "bradwell-dev"   :host "bradwell-dev"   :db "bradwell" :user "postgres" :port 15432)
          (:name "bradwell-prod"  :host "bradwell-prod"  :db "bradwell" :user "postgres" :port 25432)))

  (defun bob/ejc--create-connection (spec user password)
    "Register SPEC with ejc-sql using USER and PASSWORD."
    (let ((name (plist-get spec :name))
          (db   (plist-get spec :db))
          (port (or (plist-get spec :port) 5432)))
      (ejc-create-connection
       name
       :dependencies '[[org.postgresql/postgresql "42.7.4"]]
       :classname "org.postgresql.Driver"
       :subprotocol "postgresql"
       :subname (format "//localhost:%s/%s" port db)
       :user user
       :password password)))

  (defun bob/ejc--register-default (spec)
    "Register SPEC using its hardcoded password, if any."
    (when-let* ((pw (plist-get spec :password)))
      (bob/ejc--create-connection spec (plist-get spec :user) pw)))

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

  (defun bob/ejc--register-default-connections ()
    "Register hardcoded connections without reading authinfo."
    (mapc #'bob/ejc--register-default bob/ejc-connections))

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

  (bob/ejc--register-default-connections)

  ;; --- Corfu/CAPF bridge + ElDoc ----------------------------------------
  (defun bob/ejc--capf-candidates (&optional after-dot)
    "Return ejc-sql candidates for completion at point.
When AFTER-DOT is non-nil, only return column candidates (ejc has
already scoped them to the table/alias before the dot).  Otherwise
return the full merged set of ANSI words, DB keywords, owners,
tables, views, packages, and unscoped columns."
    (delete-dups
     (if after-dot
         (or (ejc-colomns-candidates) '())
       (append (or (ejc-get-ansi-sql-words) '())
               (or (ejc-get-keywords) '())
               (or (ejc-owners-candidates) '())
               (or (ejc-tables-candidates) '())
               (or (ejc-views-candidates) '())
               (or (ejc-packages-candidates) '())
               (or (ejc-colomns-candidates) '())))))

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
               (lambda (_prefix) (bob/ejc--capf-candidates dot-pos)))
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

  (defun bob/ejc--port-open-p (port)
    "Return non-nil when localhost PORT is reachable."
    (ignore-errors
      (let ((p (open-network-stream "ejc-port-check" nil "localhost" port)))
        (delete-process p)
        t)))

  (defun bob/ejc--wait-for-localhost-port (port on-ready)
    "Poll localhost PORT and call ON-READY once reachable."
    (let ((tries 0)
          timer)
      (setq timer
            (run-at-time
             0.5 0.5
             (lambda ()
               (setq tries (1+ tries))
               (if (bob/ejc--port-open-p port)
                   (progn
                     (cancel-timer timer)
                     (funcall on-ready))
                 (when (> tries 120)
                   (cancel-timer timer)
                   (message "Timed out waiting for localhost:%s" port))))))))

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

  (defun bob/ejc--aws-credential-error-p (text)
    "Return non-nil when TEXT looks like an expired/missing AWS SSO credential."
    (string-match-p
     (rx (or "No valid credential" "InvalidGrantException"
             "refresh cached SSO token" "SSO token"))
     text))

  (defun bob/ejc--aws-sso-login-sync (profile)
    "Run `aws sso login' for PROFILE synchronously."
    (let ((buf (get-buffer-create (format "*aws-sso:%s*" profile))))
      (with-current-buffer buf (erase-buffer))
      (message "AWS SSO login required for %s; starting browser login (see %s)"
               profile (buffer-name buf))
      (let ((status (call-process "aws" nil buf t "sso" "login" "--profile" profile)))
        (unless (and (integerp status) (zerop status))
          (error "AWS SSO login failed for %s (see %s)" profile (buffer-name buf))))))

  (defun bob/ejc--terraform-output (root env output &optional fallback)
    "Return Terraform OUTPUT for ENV under ROOT, or FALLBACK when unavailable."
    (let* ((default-directory (expand-file-name
                               (format "infrastructure/environments/%s/" env)
                               root))
           (profile (bob/ejc--aws-profile env))
           (process-environment
            (cons (format "AWS_PROFILE=%s" profile)
                  process-environment)))
      (cl-labels
          ((read-output ()
             (with-temp-buffer
               (let ((status (call-process "terraform" nil t nil "output" "-raw" output)))
                 (list status (string-trim (buffer-string))))))
           (read-output-or-error ()
             (pcase-let ((`(,retry-status ,retry-text) (read-output)))
               (cond
                ((and (integerp retry-status) (zerop retry-status)) retry-text)
                (fallback fallback)
                (t (error "terraform output -raw %s failed: %s" output retry-text)))))
           (terraform-init ()
             (let* ((account-id (bob/ejc--aws-output
                                 "sts" "get-caller-identity"
                                 "--query" "Account"
                                 "--output" "text"
                                 "--profile" profile))
                    (account-prefix (substring account-id 0 4))
                    (state-bucket (format "bradwell-terraform-state-%s-%s"
                                          env account-prefix)))
               (with-temp-buffer
                 (let ((status (call-process
                                "terraform" nil t nil
                                "init" "-input=false" "-reconfigure"
                                (format "-backend-config=bucket=%s" state-bucket))))
                   (unless (and (integerp status) (zerop status))
                     (error "terraform init failed: %s" (string-trim (buffer-string)))))))))
        (pcase-let ((`(,status ,text) (read-output)))
          (cond
           ((and (integerp status) (zerop status)) text)
           ((bob/ejc--aws-credential-error-p text)
            (bob/ejc--aws-sso-login-sync profile)
            (read-output-or-error))
           ((or (not (file-directory-p ".terraform"))
                (string-match-p "Backend initialization required" text))
            (terraform-init)
            (read-output-or-error))
           (fallback fallback)
           (t (error "terraform output -raw %s failed: %s" output text)))))))

  (defun bob/ejc--ssm-parameter (env path)
    "Return decrypted SSM parameter PATH for ENV."
    (bob/ejc--aws-output
     "ssm" "get-parameter"
     "--name" path
     "--with-decryption"
     "--query" "Parameter.Value"
     "--output" "text"
     "--profile" (bob/ejc--aws-profile env)))

  (defun bob/ejc--register-env-credentials (env)
    "Register bradwell ENV, preferring authinfo and falling back to SSM."
    ;; Drop any cached auth-source result so a freshly-edited authinfo entry
    ;; (or one decrypted after the previous lookup) is actually picked up.
    (auth-source-forget-all-cached)
    (let* ((conn (format "bradwell-%s" env))
           (spec (or (bob/ejc--connection-spec conn)
                     (user-error "No ejc connection spec for %s" conn)))
           (entry (bob/ejc--authinfo-entry conn))
           (authinfo-password (bob/ejc--authinfo-password entry)))
      (if authinfo-password
          (bob/ejc--create-connection
           spec
           (or (and entry (plist-get entry :user)) (plist-get spec :user))
           authinfo-password)
        (let ((password (bob/ejc--ssm-parameter env (format "/bradwell/%s/db-password" env)))
              (user (or (ignore-errors
                          (bob/ejc--ssm-parameter env (format "/bradwell/%s/rds-username" env)))
                        (plist-get spec :user))))
          (bob/ejc--create-connection spec user password)))))

  (defun bob/ejc--open-connection-for-buffer (conn origin)
    "Connect ORIGIN to CONN when it is a SQL buffer, else open a CONN buffer."
    (if (and (buffer-live-p origin)
             (with-current-buffer origin (derived-mode-p 'sql-mode)))
        (progn
          (with-current-buffer origin
            (unless (bound-and-true-p ejc-sql-mode) (ejc-sql-mode 1))
            (ejc-connect conn))
          (pop-to-buffer origin))
      (bob/ejc-open conn)))

  (defun bob/ejc--start-db-connect (env conn buf root origin)
    "Start the ENV database tunnel in BUF from ROOT, then connect ORIGIN to CONN."
    (let* ((spec (or (bob/ejc--connection-spec conn)
                     (user-error "No ejc connection spec for %s" conn)))
           (port (or (plist-get spec :port) 5432)))
      (if (bob/ejc--port-open-p port)
          (progn
            (message "localhost:%s is already open; using existing %s tunnel" port conn)
            (bob/ejc--register-default-connections)
            ;; Re-register remote envs so stale in-memory connection
            ;; definitions (e.g. registered before authinfo was loaded) are replaced.
            (bob/ejc--register-env-credentials env)
            (bob/ejc--open-connection-for-buffer conn origin))
        (let* ((instance-id (bob/ejc--terraform-output root env "ec2_instance_id"))
               (rds-endpoint (bob/ejc--terraform-output root env "rds_endpoint"))
               (region (bob/ejc--terraform-output root env "region" "us-east-1"))
               (profile (bob/ejc--terraform-output root env "aws_profile" (bob/ejc--aws-profile env)))
               (params (format "{\"host\":[\"%s\"],\"portNumber\":[\"5432\"],\"localPortNumber\":[\"%s\"]}"
                               rds-endpoint port)))
          (with-current-buffer buf (erase-buffer))
          (make-process
           :name (format "db-connect-%s" env)
           :buffer buf
           :command (list "aws" "ssm" "start-session"
                          "--target" instance-id
                          "--region" region
                          "--profile" profile
                          "--document-name" "AWS-StartPortForwardingSessionToRemoteHost"
                          "--parameters" params)
           :noquery t
           :connection-type 'pipe)
          (message "Started %s DB tunnel on localhost:%s (see %s)"
                   conn port (buffer-name buf))
          (bob/ejc--wait-for-localhost-port
           port
           (lambda ()
             (bob/ejc--register-default-connections)
             ;; Always re-register remote envs so stale in-memory connection
             ;; definitions from older module loads are replaced.
             (bob/ejc--register-env-credentials env)
             (bob/ejc--open-connection-for-buffer conn origin)))))))

  (defun bob/ejc-connect-env (env)
    "Ensure AWS SSO, start an SSM tunnel for ENV, then connect with ejc."
    (interactive "sENV (dev/prod): ")
    (let* ((env (downcase env))
           (conn (format "bradwell-%s" env))
           (root (file-name-as-directory
                  (expand-file-name bob/bradwell-monorepo-root)))
           (origin (current-buffer))
           (buf (get-buffer-create (format "*db-connect:%s*" env))))
      (unless (member env '("dev" "prod"))
        (user-error "ENV must be dev or prod"))
      (bob/ejc--ensure-aws-sso
       env
       (lambda ()
         (condition-case err
             (progn
               (bob/ejc--register-default-connections)
               ;; Always re-register remote envs so stale in-memory connection
               ;; definitions from older module loads are replaced.
               (bob/ejc--register-env-credentials env)
               (bob/ejc--start-db-connect env conn buf root origin))
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
    (lambda () (interactive) (bob/ejc-open "bradwell-local")))

(provide 'bob-ejc-sql)
;;; bob-ejc-sql.el ends here
