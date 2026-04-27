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
;; Connection passwords are read from ~/.authinfo.gpg.  A connection is
;; only registered when authinfo has a matching `machine' line — so to
;; add dev or prod you just append a line and run
;; `M-x bob/ejc-refresh-connections'.
;;
;;   machine bradwell-local login postgres password postgres port 5432
;;   machine bradwell-dev   login bradwell password <pw>     port 5432
;;   machine bradwell-prod  login bradwell password <pw>     port 5432
;;
;; Remote (dev/prod) connections assume a local SSM tunnel on
;; localhost:5432, started by `task db-connect ENV=dev' (or
;; `ENV=prod') in the monorepo.
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
  ;; Each connection is a plist: :name :host :db :user.  The password
  ;; is always pulled from ~/.authinfo.gpg at load time via
  ;; `bob/ejc--register'.  Defaults match the local minikube pod and
  ;; the RDS schema used by services/migrations.  Override user/db
  ;; per-env by editing the authinfo line (auth-source reads :user too).
  (defvar bob/ejc-connections
    '((:name "bradwell-local" :host "bradwell-local" :db "bradwell" :user "postgres")
      (:name "bradwell-dev"   :host "bradwell-dev"   :db "bradwell" :user "bradwell")
      (:name "bradwell-prod"  :host "bradwell-prod"  :db "bradwell" :user "bradwell"))
    "Bradwell ejc-sql connections.
Each entry is auto-registered iff a matching machine line exists
in ~/.authinfo.gpg.")

  (defun bob/ejc--register (spec)
    "Register the connection described by plist SPEC if authinfo has a password."
    (let* ((name (plist-get spec :name))
           (host (plist-get spec :host))
           (db   (plist-get spec :db))
           (default-user (plist-get spec :user))
           (entry (bob/ejc--authinfo-entry host))
           (pw    (bob/ejc--authinfo-password entry))
           (user  (or (and entry (plist-get entry :user)) default-user)))
      (when pw
        (ejc-create-connection
         name
         :dependencies '[[org.postgresql/postgresql "42.7.4"]]
         :classname "org.postgresql.Driver"
         :subprotocol "postgresql"
         ;; All remote connections are expected to be tunneled to localhost:5432
         ;; (local minikube port-forward; `task db-connect ENV=<env>` for RDS).
         :subname (format "//localhost:5432/%s" db)
         :user user
         :password pw))))

  (defun bob/ejc-registered-connections ()
    "Names of connections currently registered with ejc-sql."
    (when (boundp 'ejc-connections)
      (mapcar #'car ejc-connections)))

  (defun bob/ejc-refresh-connections ()
    "Re-read ~/.authinfo.gpg and register any bradwell-* connections found."
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
                         (error "No ejc-sql connections registered. Add a line to ~/.authinfo.gpg, then M-x bob/ejc-refresh-connections")))
            (default (if (member "bradwell-local" choices)
                         "bradwell-local"
                       (car choices))))
       (list (completing-read (format "ejc connection (default %s): " default)
                              choices nil t nil nil default))))
    (let* ((name (or connection "bradwell-local"))
           (bufname (format "*ejc: %s*" name))
           (buf (get-buffer-create bufname)))
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

  (defun bob/ejc-connect-env (env)
    "Run `task db-connect ENV=ENV` asynchronously, then open ejc buffer."
    (interactive "sENV (dev/prod): ")
    (let* ((env (downcase env))
           (conn (format "bradwell-%s" env))
           (default-directory (file-name-as-directory
                               (expand-file-name bob/bradwell-monorepo-root)))
           (buf (get-buffer-create (format "*db-connect:%s*" env))))
      (unless (member env '("dev" "prod"))
        (user-error "ENV must be dev or prod"))
      (with-current-buffer buf (erase-buffer))
      (make-process
       :name (format "db-connect-%s" env)
       :buffer buf
       :command (list "task" "db-connect" (format "ENV=%s" env))
       :noquery t
       :connection-type 'pipe)
      (message "Started task db-connect ENV=%s (see %s)" env (buffer-name buf))
      (bob/ejc-refresh-connections)
      (bob/ejc--wait-for-localhost-5432
       (lambda ()
         (bob/ejc-refresh-connections)
         (bob/ejc-open conn)))))

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
