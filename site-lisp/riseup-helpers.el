(defun browse-riseup-git-project ()
    "Browse a riseup git repositary using the current known project for completion"
    (interactive)
    (browse-url (format "http://github.com/riseupil/%s"
                        (car (last (cl-remove-if #'seq-empty-p
                                                 (s-split "/"
                                                          (completing-read "select riseup repo"
                                                                           (get--project-names project--list)))))))
                t))

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
  (let ((selected-repo (completing-read "select riseup repo"
                                        (read-file riseup-repos-cache-path))))
    (magit-clone-internal (format "%s/%s.git" "git@github.com:riseupil" selected-repo)
                          service-directory
                          nil)))

(defun save--riseup-repo-names-to-cache (file-name)
  (with-temp-file file-name
    (insert (to-string (get-all-riseup-repos)))))

(provide 'riseup-helpers)
