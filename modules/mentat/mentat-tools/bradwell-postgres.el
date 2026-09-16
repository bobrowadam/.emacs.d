;;; bradwell-postgres.el --- Read-only Bradwell PostgreSQL access -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'auth-source)
(require 'json)
(require 'subr-x)
(require 'bob-auth-process)
(require 'mentat-emacs)
(require 'mentat-elisp-library)

(defconst mentat-bradwell-postgres--max-bytes (* 8 1024 1024))

(defun mentat-bradwell-postgres--validate-sql (sql)
  "Validate SQL without exposing it in diagnostics."
  (unless (and (stringp sql) (not (string-empty-p (string-trim sql)))
               (<= (string-bytes sql) 200000))
    (user-error "SQL must be a nonempty bounded string"))
  (when (or (string-match-p (regexp-quote (char-to-string 59)) sql)
            (string-match-p (regexp-quote (concat (char-to-string 45) (char-to-string 45))) sql)
            (string-match-p (regexp-quote (concat (char-to-string 47) (char-to-string 42))) sql)
            (string-match-p (regexp-quote (concat (char-to-string 42) (char-to-string 47))) sql)
            (string-match-p (regexp-quote (char-to-string 92)) sql))
    (user-error "SQL must contain one expression with no semicolons, comments, or psql commands"))
  (let ((case-fold-search t))
    (unless (string-match-p "\\`[[:space:]]*\\(?:select\\|with\\)\\_>" sql)
      (user-error "SQL must start with SELECT or WITH")))
  sql)

(defun mentat-bradwell-postgres--number (value default maximum name)
  (setq value (or value default))
  (unless (and (integerp value) (> value 0) (<= value maximum))
    (user-error "%s must be a positive integer no greater than %d" name maximum))
  value)

(defconst mentat-bradwell-postgres--script
  (concat
   "set -eu\n"
   "Q=$1; ROOT=$2; ENV=$3; MAX=$4; FETCH=$5; TIMEOUT=$6; LOCK=$7\n"
   "IN=$Q/input.sql; EX=$Q/execute.sql; OUT=$Q/result.json; LOG=$Q/error.log; TUNNEL=; PSQL=\n"
   "die() { printf '%s\n' 'Bradwell PostgreSQL operation failed' >&2; exit 1; }; clean() { test -z $PSQL || kill $PSQL 2>/dev/null || :; test -z $TUNNEL || kill $TUNNEL 2>/dev/null || :; rm -rf $Q; }; trap clean EXIT INT TERM; cat > $IN || die\n"
   "D=$ROOT/infrastructure/environments/$ENV; test -d $D || die; tf() { terraform -chdir=$D output -raw $1 2>/dev/null || die; }; REGION=$(tf region); PROFILE=$(tf aws_profile); INSTANCE=$(tf ec2_instance_id); HOST=$(tf rds_endpoint); DB=$(tf rds_database_name); PORT=$(tf rds_port)\n"
   "LOCAL=$(python3 -c 'import socket; s=socket.socket(); s.bind((\"127.0.0.1\",0)); print(s.getsockname()[1]); s.close()') || die; PARAMS=$(printf '{\"host\":[\"%s\"],\"portNumber\":[\"%s\"],\"localPortNumber\":[\"%s\"]}' $HOST $PORT $LOCAL)\n"
   "aws --no-cli-pager ssm start-session --target $INSTANCE --document-name AWS-StartPortForwardingSessionToRemoteHost --parameters $PARAMS --region $REGION --profile $PROFILE >$LOG 2>&1 & TUNNEL=$!; READY=0; I=0; while test $I -lt 60; do kill -0 $TUNNEL 2>/dev/null || die; if python3 -c 'import socket,sys; s=socket.create_connection((\"127.0.0.1\",int(sys.argv[1])),.2); s.close()' $LOCAL >/dev/null 2>&1; then READY=1; break; fi; I=$((I+1)); sleep .2; done; test $READY -eq 1 || die\n"
   "ROLE=$(psql -X -q -A -t -h 127.0.0.1 -p $LOCAL -U readonly -d $DB -c 'SELECT CASE WHEN rolsuper OR rolcreaterole OR rolcreatedb OR rolreplication OR rolbypassrls THEN 0 ELSE 1 END FROM pg_catalog.pg_roles WHERE rolname=current_user') || die; test $ROLE = 1 || die\n"
   "{ printf '%s\n' 'BEGIN TRANSACTION READ ONLY;'; printf '%s\n' SET\ LOCAL\ statement_timeout\ =\ ${TIMEOUT}ms; printf '%s\n' SET\ LOCAL\ lock_timeout\ =\ ${LOCK}ms; printf '%s\n' 'WITH limited AS (' 'SELECT * FROM ('; cat $IN; printf '%s\n' ') AS result_row LIMIT ' $FETCH ');'; cat <<SQL; printf '%s\n' 'COMMIT;'\n"
   "SELECT json_build_object('rows',COALESCE((SELECT json_agg(to_jsonb(returned_row)) FROM (SELECT * FROM limited LIMIT $MAX) AS returned_row),'[]'::json),'truncated',(SELECT count(*) > $MAX FROM limited));\nSQL\n"
   "} > $EX || die; if ! psql -X -q -A -t -v ON_ERROR_STOP=1 -h 127.0.0.1 -p $LOCAL -U readonly -d $DB -f $EX >$OUT 2>>$LOG; then die; fi; test $(wc -c <$OUT) -le 8388608 || die; cat $OUT\n")
  "One cleaned-up shell operation owns Terraform, SSM, and psql.")

(mentat--elisp-register-library 'bradwell-postgres 'user
 "Run bounded read-only Bradwell PostgreSQL SELECT queries through Terraform and AWS SSM.")

(mentat-defun mentat-bradwell-postgres-select
    (environment sql &key max-rows statement-timeout-ms)
  "Run one bounded read-only SELECT or WITH expression and return JSON."
  (:execution async :display "Bradwell PostgreSQL Select")
  (unless (member environment '("dev" "prod"))
    (user-error "Environment must be dev or prod"))
  (mentat-bradwell-postgres--validate-sql sql)
  (lambda (resolve reject on-cancel)
    (let* ((max-rows (mentat-bradwell-postgres--number max-rows 100 500 "max-rows"))
         (timeout (mentat-bradwell-postgres--number statement-timeout-ms 30000 120000 "statement-timeout-ms"))
         (lock (min timeout 5000))
         (root (expand-file-name default-directory))
         (infra (expand-file-name (format "infrastructure/environments/%s" environment) root))
         (dir (make-temp-file "bradwell-postgres-" t))
         (buffer (generate-new-buffer " *bradwell-postgres-process*"))
         (host (if (string-equal environment "dev") "postgres.dev.bradwell.internal" "postgres.prod.bradwell.internal"))
         (args (list "-c" mentat-bradwell-postgres--script "--" dir root environment (number-to-string max-rows) (number-to-string (1+ max-rows)) (number-to-string timeout) (number-to-string lock)))
         (credentials (list (list "PGPASSWORD" host "readonly"))) process settled output)
    (unless (file-directory-p infra)
      (delete-directory dir t) (kill-buffer buffer)
      (user-error "Bradwell infrastructure directory is unavailable"))
    (unless (auth-source-search :max 1 :host host
                                :user "readonly"
                                :require '(:secret))
      (delete-directory dir t) (kill-buffer buffer)
      (user-error "Dedicated read-only PostgreSQL credential is unavailable for %s" environment))
    (cl-labels
        ((cleanup () (when (process-live-p process) (delete-process process)) (when (buffer-live-p buffer) (kill-buffer buffer)) (when (file-directory-p dir) (ignore-errors (delete-directory dir t))))
         (cancel () (setq settled t) (cleanup))
         (collect (_ chunk) (unless settled (setq output (mentat--utf8-truncate (concat output chunk) mentat-bradwell-postgres--max-bytes "…"))))
         (finish (child _event)
           (when (and (not settled) (memq (process-status child) '(exit signal)))
             (setq settled t)
             (let ((ok (and (eq (process-status child) 'exit) (zerop (process-exit-status child)))) (result (string-trim output)))
               (cleanup)
               (if (not ok) (funcall reject "Bradwell PostgreSQL operation failed")
                 (condition-case _err (progn (json-parse-string result :object-type 'alist) (funcall resolve result))
                   (error (funcall reject "Bradwell PostgreSQL returned invalid JSON"))))))))
      (funcall on-cancel #'cancel)
      (condition-case _err
          (progn (setq process (bob/start-process-with-credentials "/bin/sh" args credentials :directory root :buffer buffer :filter #'collect :sentinel #'finish)) (process-send-string process sql) (process-send-eof process))
        (error (setq settled t) (cleanup) (funcall reject "Bradwell PostgreSQL operation could not start")))))))

(provide 'bradwell-postgres)
;;; bradwell-postgres.el ends here
