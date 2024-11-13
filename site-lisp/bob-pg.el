(require 'pg)

(setq-local *bob/pg-connection-args* '("grain" "postgres" "grain"))

(defun bob-pg/read-tables ()
  (completing-read "Pick table: "
                   (bob/with-pg-connection conn *bob/pg-connection-args*
                     (pg-tables conn))))

(defun run-with-table-parser (query)
  (let* ((result (bob/with-pg-connection conn *bob/pg-connection-args*
                   (pg-exec conn
                            query)))
         (attributes (mapcar 'car (pg-result
                                   result
                                   :attributes)))
         (rows (pg-result
                result
                :tuples)))
    (cl-loop for row in rows
             collect (-zip attributes
                           row))))

(mapcar (λ (list :email (assocdr "email" %1)
                 :role (assocdr "role" %1)))
        (run-with-table-parser "SELECT * from admin_console_users LIMIT 1;"))

(defmacro bob/with-pg-connection (con connect-args &rest body)
  "Execute BODY forms in a scope with connection CON created by CONNECT-ARGS.
The database connection is bound to the variable CON. If the
connection is unsuccessful, the forms are not evaluated.
Otherwise, the BODY forms are executed, and upon termination,
normal or otherwise, the database connection is closed."
  `(if-let ((,con (pg-connect ,@(eval connect-args))))
       (unwind-protect
              (progn ,@body)
            (pg-disconnect ,con))
     (error "Connection failed")))

(defun condition-builder (condition)
  "build SQL queries from a list DSL"
  (cond
   ((where-condition-p condition) 0)
   (t 1)))

(defun where-condition-p (condition)
  (assoc))
(-contains-p '(where) 'where)

(defun find-many (table condition)
  (format "SELECT * from %s where %s" table (condition-builder condition)))

(provide 'bob-pg)


(bob/with-pg-connection connection *bob/pg-connection-args*
                   (pg-exec connection
                            "SELECT * from partner_configs;"))

(defmacro test-m (args)
  `(print ,(eval args)))

(test-m *bob/pg-connection-args*)
