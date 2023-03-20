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
                                        (read-file riseup-repos-cache-path))))
    (magit-clone-internal (format "%s/%s.git" "git@github.com:riseupil" selected-repo)
                          service-directory
                          nil)))

(defun save--riseup-repo-names-to-cache (file-name)
  (with-temp-file file-name
    (insert (to-string (get-all-riseup-repos)))))

(defun import-customer (customer-id)
  (interactive "N")
  (-let [default-directory (format "%s/source/catapult" (getenv "HOME"))]
    (async-shell-command (format "%s %s %s %s"
                                 (fnm-node-path "18")
                                 "./node_modules/env-setter/src/ssm-entrypoint-local.js local.js"
                                 "customer-import-locally"
                              customer-id))))

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

(defun browse-data-dog-customer (&optional project)
  "Browse riseup customer in mamadmin"
  (interactive)
  (let ((customer-id (read-number "Enter customer:\n")))
    (browse-url (format "https://riseup.datadoghq.com/logs?query=customerId%%3A%s"
                        customer-id)
                t)))

(defun browse-data-dog-text (&optional project)
  "Browse riseup customer in mamadmin"
  (interactive)
  (let ((query-text (read-string "Enter text:\n")))
    (browse-url (format "https://riseup.datadoghq.com/logs?query=%s"
                        query-text)
                t)))
(defun browse-data-dog-service (&optional project)
  "Browse riseup customer in mamadmin"
  (interactive)
  (let ((service-name (read-string "Enter service name:\n")))
    (browse-url (format "https://riseup.datadoghq.com/logs?query=%%40service%%3A%s"
                        service-name)
                t)))

(transient-define-prefix data-dog-jump ()
  "Search in data dog"
  ["Data Dog Jump"
   [("c " "Customer" browse-data-dog-customer)]
   [("s " "Service" browse-data-dog-service)]
   [("t " "Text" browse-data-dog-text)]])

(transient-define-prefix mamadmin-jump ()
  "Search in data dog"
  ["Mamadmin Customer"
   [("c " "Customer Home" browse-customer-in-mamadmin)]
   [("m " "Customer Merge Screen" browse-customer-merge-in-mamadmin)]])

(transient-define-prefix riseup-actions ()
  "Search in data dog"
  ["Data Dog Jump"
   [("m " "Mamadmin Actions" mamadmin-jump)]
   [("d " "Data Dog Actions" data-dog-jump)]])

(bind-key "C-c C-d" 'riseup-actions)

(provide 'riseup-helpers)
