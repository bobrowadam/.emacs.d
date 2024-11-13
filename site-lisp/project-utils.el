(defun bobs/project-read-file-name-function (prompt
                                             all-files &optional predicate
                                             hist mb-default)
  (let* ((display-name-to-file-path-hash-map (display-name-to-file-path all-files (round (* 0.95 (frame-width))))))
    (gethash (completing-read (concat prompt ": ")
                              display-name-to-file-path-hash-map
                              predicate
                              t
                              nil
                              hist)
             display-name-to-file-path-hash-map)))

(defun file-name-prefix (file-path max-file-size)
  (let* ((file-name (file-name-nondirectory file-path))
        (space-padding-after-file-name (make-string (min 5 (- max-file-size (length file-name)))
                                                    ?\s)))
   (format "%s %s%s"
           file-name
           space-padding-after-file-name
           (propertize "➜" 'face 'modus-themes-heading-0))))

(defun longest--file-name (files)
  (file-name-nondirectory (-max-by (lambda (f1 f2)
                                     (> (length (file-name-nondirectory f1))
                                        (length (file-name-nondirectory f2))))
                                   files)))

(defun display-name-to-file-path (files max-length)
  (let ((names-to-path-map (make-hash-table :test 'equal))
        (longest-file-name-length (length (longest--file-name files))))
    (dolist (file files)
      (let* ((file-name-prefixed (file-name-prefix file longest-file-name-length))
             (truncate-path-name (bobs/truncate--path-name (s-chop-prefix (expand-file-name (project-root (project-current)))
                                                                          file)
                                                           (- max-length (length file-name-prefixed))))
             (displayed-file-name (format "%s %s"
                                          file-name-prefixed
                                          truncate-path-name))
             (key-exists-p (gethash displayed-file-name names-to-path-map))
             (unique-displayed-file-name (if key-exists-p
                                             (format "%s<%s> path depth: %s>"
                                                     displayed-file-name
                                                     (file-name-directory file)
                                                     (length file))
                                           displayed-file-name)))
        (puthash unique-displayed-file-name
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
         (symlink-target-dir (format "%sriseup/rest-clients" org-directory)))
    (if (file-exists-p rest-client-path)
        (find-file rest-client-path)
      (let ((chosen-file (expand-file-name (read-file-name "Choose org file to symlink: " symlink-target-dir))))
        (when chosen-file
          (make-symbolic-link chosen-file rest-client-path)
          (find-file rest-client-path))))))

(defun override-project-prompt-project-dir ()
  (progn
    (defun parent-directory-name (dir-path display-name-to-item-hash-table)
      (let* ((file-name-nondirectory (directory-file-name (file-name-directory dir-path)))
             (hash-colition (gethash file-name-nondirectory display-name-to-item-hash-table)))
          (if hash-colition
              (progn
                (error "hash collision %s" hash-colition)
                (format "%s-(%s)"
                       file-name-nondirectory
                       (cl-random 100)))
            file-name-nondirectory)))

    (defun project-prompt-project-dir ()
      "Prompt the user for a directory that is one of the known project roots.
The project is chosen among projects known from the project list,
see `project-list-file'.
It's also possible to enter an arbitrary directory not in the list."
      (project--ensure-read-project-list)
      (let* ((dir-choice "... (choose a dir)")
             (choices
              ;; XXX: Just using this for the category (for the substring
              ;; completion style).
              (mapcar 'car project--list))
             (pr-dir ""))
        (while (equal pr-dir "")
          ;; If the user simply pressed RET, do this again until they don't.
          (setq pr-dir (bob/completing-read "Select project: " choices #'parent-directory-name)))
        (if (equal pr-dir dir-choice)
            (read-directory-name "Select directory: " default-directory nil t)
          pr-dir)))))

(defun bob/project--relevant-buffer-p (buf)
  (and (buffer-file-name buf)
       (not (eq buf (current-buffer)))))

(defun bob/project-switch-buffer ()
  "Switch to a project buffer in the current project."
  (interactive)
  (let ((project (project-current)))
    (unless project
      (error "Not in a project"))
    (let ((buffers (seq-filter 'bob/project--relevant-buffer-p (project-buffers project))))
      (switch-to-buffer
       (completing-read "Switch to buffer: "
                        (lambda (string predicate action)
                          (if (eq action 'metadata)
                              '(metadata (category . buffer))
                            (complete-with-action action
                                                  (mapcar #'buffer-name buffers)
                                                  string
                                                  predicate))))))))

(defun bob/monorepo-root ()
  "Finds the topmost root in a multi-project structure."
  (or (-some--> (project-current nil (file-name-parent-directory default-directory))
        project-root
        (let ((default-directory it))
          (bob/monorepo-root)))
      (-some-> (project-current) project-root)))

(defun bob/eat-top-project ()
  "Open an Eat shell on the highest project"
  (interactive)
  (if-let* ((project--root (bob/monorepo-root))
             (default-directory project--root))
      (eat-project)
    (error "Not in project")))

(defun bob/switch-to-recent-buffer ()
  "Switch to the most recently created visible buffer."
  (interactive)
  (let ((recent-buffers (mapcar #'window-buffer (window-list))))
    (if recent-buffers
        (let ((recent-buffer (car (last recent-buffers))))
          (pop-to-buffer-same-window recent-buffer))
      (message "No visible buffers found."))))

(defun bob/project-list-buffers ()
  (interactive)
  (project-list-buffers t)
  (other-window 1))

(provide 'project-utils)
