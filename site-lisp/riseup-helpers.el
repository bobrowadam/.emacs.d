;;; riseup-helpers.el ---  -*- lexical-binding: t -*-
(require 'cl-lib)
(require 'ghub)
(require 'mongo)

(defun browse-riseup-git-project (&optional project)
  "Browse a riseup git repository using the current known project for completion"
  (interactive)
  (let ((current-project-name (or project (car (last (cl-remove-if #'seq-empty-p
                                                                   (s-split "/"
                                                                            (completing-read "select riseup repo:\n"
                                                                                             (get--project-names project--list)))))))))
    (browse-url (format "http://github.com/riseupil/%s"
                        current-project-name)
                t)))

(defun get--project-names (projects-paths)
  (mapcar (lambda (project-path)
            (car (last (cl-remove-if #'seq-empty-p
                                              (s-split "/"
                                                       (car project-path))))))
          projects-paths))

(defun get-all-riseup-repos ()
  (message "fetching for all riseup repos")
    (let ((first-page 0)
          (repos '()))
      (while-let ((page-number (+ 1 first-page))
                  (page-repos (get--repos-name (ghub-get "orgs/riseupil/repos" `((page . ,page-number)
                                                                                 (per_page . 100))
                                                         :username "bobrowadam"))))
        (print "finished calling git:")
        (print page-repos)
        (setq first-page
              (+ 1 first-page))
        (setq repos
              (seq-concatenate 'list repos page-repos)))
      repos))

(defun get--repos-name (repos)
  (mapcar (lambda (repo)
            (cdr (assoc 'name repo)))
        repos))


(defconst riseup-repos-cache-path "~/.emacs.d/.riseup-repos-cache.el"
  "A file path for saving riseup's repository names.
This is used for 'clone-riseup-repo'")

(defun clone-riseup-repo (&optional refresh-cache)
  "Clone a riseup repo into default dir"
  (interactive "P")
  (when (car refresh-cache)
    (save--riseup-repo-names-to-cache riseup-repos-cache-path))
  (let ((selected-repo (completing-read "select riseup repository\n"
                                        (read (read-file riseup-repos-cache-path)))))
    (magit-clone-internal (format "%s/%s.git" "git@github.com:riseupil" selected-repo)
                          service-directory
                          nil)))

(defun save--riseup-repo-names-to-cache (file-name)
  (with-temp-file file-name
    (insert (to-string (get-all-riseup-repos)))))

(defun import-customer (&optional dont-backup)
  (interactive "P")
  (let ((customer-id (read-number "Enter customer id:\n")))
   (unless dont-backup
     (browse-url (format "https://mamadmin.riseup.co.il/#/home/customer/%s/backup" customer-id)))
   (sleep-for 13)
   (let ((default-directory (format "%s/source/services/catapult" (getenv "HOME"))))
     (async-shell-command (format "npm run import-customer %s" customer-id)
                          "*import-customer-output-buffer*"
                          "*import-customer-error-buffer*"))))

(defun browse-customer-in-mamadmin (&optional project)
  "Browse riseup customer in mamadmin"
  (interactive)
  (let ((customer-id (read-number "Enter customer:\n")))
    (browse-url (format "https://mamadmin.riseup.co.il/#/home/customer/%s/"
                        customer-id)
                t)))

(defun browse-customer-merge-in-mamadmin (&optional project)
  "Browse riseup customer in mamadmin"
  (interactive)
  (let ((customer-id (read-number "Enter customer:\n")))
    (browse-url (format "https://mamadmin.riseup.co.il/#/home/customer/%s/data"
                        customer-id)
                t)))

(defun browse-data-dog-dwim ()
  "Browse Data Dog.
When entering a service name, look for the service.
When entering a number, look for a customerId.
Else, just look for the given string."
  (interactive)
  (let* ((riseup-repos (read (read-file riseup-repos-cache-path)))
         (query-string (completing-read "Enter datadog query:\n"
                                        riseup-repos)))
    (let ((generated-datadog-url (cond ((-contains-p riseup-repos (intern query-string))
                                        (format "https://riseup.datadoghq.com/logs?query=%%40service%%3A%s" query-string))
                                       ((s-numeric-p query-string)
                                        (format "https://riseup.datadoghq.com/logs?query=@customerId%%3A%s"
                                                (string-to-number query-string)))
                                       (t (format "https://riseup.datadoghq.com/logs?query=%s" query-string)))))
      (browse-url generated-datadog-url t))))

(defun list-possible-customers ()
  (mapcar
   (lambda (document) (to-string (assocdr "_id" document)))
   (let* ((result
           (mongo-with-open-database
               (db :host "localhost" :port 27017) ; Specify host and port, if needed
             (mongo-do-request
              (make-mongo-message-query
               :flags 0
               :number-to-skip 0
               :number-to-return 0 ; Set to a positive integer to limit the number of documents returned
               :full-collection-name "mamadmin.customers"
               :query '())       ; An empty query to fetch all documents
              :database db)))
          (documents (mongo-message-reply-documents result))) ; Extract the documents from the result
     documents)))

(defun run-customer-version ()
  "Run a riseup customer version locally"
  (interactive)
  (let ((customer-id (string-to-number (completing-read "Select customer ID: " (list-possible-customers) nil nil))))
    (request "http://host.docker.internal:4030/dev/trigger-version-update"
      :method "POST"
      :data (json-encode
             `((customerId . ,customer-id)
               (user . ((name . "bob")))))
      :headers '(("Content-Type" . "application/json"))
      :parser 'json-read
      :success (cl-function
                (lambda (&key response &allow-other-keys)
                  (message "response: %S" response)
                  (message "Version request for customer %s finished with status code %s"
                           customer-id
                           (request-response-status-code response))))

      :error (cl-function
              (lambda (&key error-thrown &allow-other-keys)
                (error "error when running version for customer %S. Error: %S"
                       customer-id
                       error-thrown))))))

(defun init-riseup-actions ()
  (progn
    (transient-define-prefix data-dog-jump ()
      "Search in data dog"
      ["Data Dog Jump"
       [("c " "Customer" browse-data-dog-customer)]
       [("s " "Service" browse-data-dog-service)]
       [("t " "Text" browse-data-dog-text)]])

    (transient-define-prefix customer-actions ()
      "Customer actions"
      ["Customer functions"
       [("v " "Run customer's version" run-customer-version)]
       [("c " "Customer Home" browse-customer-in-mamadmin)]
       [("m " "Customer Merge Screen" browse-customer-merge-in-mamadmin)]
       [("I " "Import customer from prod" import-customer)]])

    (transient-define-prefix riseup-actions ()
      "Run Riseup actions"
      ["Riseup actions"
       [("m " "Customer Actions" customer-actions)]
       [("d " "Data Dog DWIM" browse-data-dog-dwim)]
       [("s " "Search for service by port number" search-for-riseup-service-by-port)]])))

(defconst *general-env* "/Users/bob/source/bob/config_mgmt/local/general.env")
(defun map--env-file-to-port->service (file-name)
  "Returns an Alist of ((<port-number> . <service-name>))"
    (let ((env-file-content (to-string (read-file file-name))))
     (mapcar (lambda (service-name=port)
               (cl-destructuring-bind (service port) (s-split "=" service-name=port)
                 (cons port (s-replace "_PORT" "" service))))
             (-filter (lambda (line) (s-contains? "PORT" line))
                      (s-split "\n" env-file-content)))))

(defun search-for-riseup-service-by-port ()
  "Search a riseup service by port number."
  (interactive)
  (let* ((port-to-service-map (map--env-file-to-port->service *general-env*))
         (port-names (mapcar (lambda (service-to-port)
                                  (car service-to-port))
                                port-to-service-map))
         (service-port (completing-read "Choose a port to search for:\n" port-names)))
    (message "Service for port %s is %s"
             service-port
             (cdr (assoc service-port port-to-service-map)))))

(defun is-typescript-project ()
  "Returns true when project has an NPM build script for typescript"
  (if-let* ((project (project-current))
            (default-directory (project-root project))
            (package-json-raw (read-file "package.json"))
            (package-json (json-parse-string package-json-raw
                                             :object-type 'alist)))
      (s-matches-p (assocdr 'build (assocdr 'scripts package-json))
               "./node_modules/typescript/bin/tsc")))

(defun riseup--dependencies ()
  "Returns a list of a riseup node project dependencies"
  (if-let* ((project (project-current))
            (default-directory (project-root project))
            (package-json (json-parse-string (read-file "package.json")
                                             :object-type 'alist)))
      (-filter (lambda (str) (s-starts-with? "@riseupil" str))
         (mapcar 'symbol-name (mapcar 'car (assocdr 'dependencies package-json))))))

(defun link-riseup-project ()
  "Use NPM link to link a @riseupil/* dependencies.
The target library should be globally linked"
  (interactive)
  (if-let ((dependency (completing-read "Pick a package to link" (riseup--dependencies))))
    (async-shell-command (format "npm link %s" dependency)
                         "*npm-link-output-buffer*"
                         "*npm-link-error-buffer*")))
(provide 'riseup-helpers)
