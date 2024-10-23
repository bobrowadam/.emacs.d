(require 'pg)

(defun bob-pg/read-tables ()
  (completing-read "Pick table: "
                   (with-pg-connection conn ("grain" "postgres" "grain" "127.0.0.1")
                     (setq *pg-connection* conn)
                     (pg-tables conn))))

(with-pg-connection conn ("grain" "postgres" "grain" "127.0.0.1")
                     (setq *pg-connection* conn)
                     (pg-exec conn "SELECT * from forwards limit 1"))

(provide 'bob-pg)
