(defun bobs/project-read-file-name-function (prompt
                                             all-files &optional predicate
                                             hist mb-default)
  (let* ((display-name-to-file-path-hash-map (display-name-to-file-path all-files 90)))
    (gethash (completing-read (concat prompt ": ") display-name-to-file-path-hash-map predicate t nil hist)
             display-name-to-file-path-hash-map)))

(defun display-name-to-file-path (files max-length)
  (let ((names-to-path-map (make-hash-table :test 'equal)))
    (dolist (file files)
      (puthash (bobs/truncate-path-name (s-chop-prefix (expand-file-name (project-root (project-current)))
                                                file)
                                        max-length)
               file
               names-to-path-map))
    names-to-path-map))

(defun bobs/truncate-path-name (path-string max-length)
    (if (< (length path-string) max-length)
        path-string
     (let* ((path-nodes (-remove-item "..." (s-split "/" path-string)))
            (paths-length (length path-nodes))
            (splited-paths (-split-at (/ paths-length 2) path-nodes))
            (first-half (car splited-paths))
            (second-half (cadr splited-paths))
            (truncated-first-half (-remove-at (length first-half) first-half))
            (truncated-second-half (-remove-at 0 second-half)))
       (bobs/truncate-path-name (s-join "/" (cl-concatenate 'list truncated-first-half '("...") truncated-second-half))
                                max-length))))

(provide 'bobs-project-utils)
