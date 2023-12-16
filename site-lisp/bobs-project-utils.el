(defun bobs/project-read-file-name-function (prompt
                                             all-files &optional predicate
                                             hist mb-default)
  (let* ((display-name-to-file-path-hash-map (display-name-to-file-path all-files (round (* 0.80 (frame-width))))))
    (gethash (completing-read (concat prompt ": ")
                              display-name-to-file-path-hash-map
                              predicate
                              t
                              nil
                              hist)
             display-name-to-file-path-hash-map)))

(defun display-name-to-file-path (files max-length)
  (let ((names-to-path-map (make-hash-table :test 'equal)))
    (dolist (file files)
      (let* ((truncate-path-name (bobs/truncate--path-name (s-chop-prefix (expand-file-name (project-root (project-current)))
                                                                         file)
                                                          max-length))
             (key-exists-p (gethash truncate-path-name names-to-path-map))
             (unique-truncate-path-name (if key-exists-p
                                            (format "%s<%s>" truncate-path-name (file-name-directory file))
                                          truncate-path-name)))
        (puthash truncate-path-name
                 file
                 names-to-path-map)))
    names-to-path-map))

(defcustom project-path-ellipses "..."
  "The ellipses shown in truncated file path."
  :group 'project)

(defun bobs/truncate--path-name (path-string max-length)
  (if (< (length path-string) max-length)
      path-string
    (let* ((path-nodes (-remove-item project-path-ellipses (s-split "/" path-string)))
           (paths-length (length path-nodes))
           (splited-paths (-split-at (/ paths-length 2) path-nodes))
           (first-half (car splited-paths))
           (second-half (cadr splited-paths))
           (truncated-first-half (-remove-at (length first-half) first-half))
           (truncated-second-half (-remove-at 0 second-half)))
      (bobs/truncate--path-name (s-join "/" (cl-concatenate 'list
                                                           truncated-first-half
                                                           `(,project-path-ellipses)
                                                           truncated-second-half))
                               max-length))))

(defun project-jump-to-rest-client ()
  (interactive)
  (let* ((project (project-current t))
         (project-root (project-root project))
         (project-name (file-name-nondirectory (directory-file-name project-root)))
         (rest-client-name (format "%s-client.org" project-name))
         (rest-client-path (expand-file-name rest-client-name project-root))
         (symlink-target-dir "/Users/bob/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/riseup/rest-clients/"))
    (if (file-exists-p rest-client-path)
        (find-file rest-client-path)
      (let ((chosen-file (expand-file-name (read-file-name "Choose org file to symlink: " symlink-target-dir))))
        (when chosen-file
          (make-symbolic-link chosen-file rest-client-path)
          (find-file rest-client-path))))))

(provide 'bobs-project-utils)
