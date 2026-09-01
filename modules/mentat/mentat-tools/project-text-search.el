;;; project-text-search.el --- Bounded project text search -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'json)
(require 'project)
(require 'seq)
(require 'subr-x)
(require 'mentat-elisp-library)

(defun mentat-text-search--bounded-line (line)
  "Return LINE without trailing carriage return, bounded to 500 characters."
  (let ((text (string-remove-suffix "\r" line)))
    (if (> (length text) 500)
        (concat (substring text 0 500) "…")
      text)))

(defun mentat-text-search--format-matches (matches)
  "Return MATCHES as compact grep-style text."
  (if (null matches)
      "No matches."
    (mapconcat
     (lambda (match)
       (let* ((file (alist-get 'file match))
              (line (alist-get 'line match))
              (before (alist-get 'before match))
              (after (alist-get 'after match))
              (before-start (- line (length before))))
         (string-join
          (append
           (cl-loop for text in before
                    for number from before-start
                    collect (format "%s-%d-%s" file number text))
           (list (format "%s:%d:%s" file line (alist-get 'text match)))
           (cl-loop for text in after
                    for number from (1+ line)
                    collect (format "%s-%d-%s" file number text)))
          "\n")))
     matches
     "\n--\n")))

(defun mentat-text-search--file
    (query file regexp max-results context case-fold display-file)
  "Search FILE for QUERY and return at most MAX-RESULTS structured matches."
  (with-temp-buffer
    (insert-file-contents file)
    (unless (search-forward "\0" nil t)
      (let* ((lines (split-string (buffer-string) "\n" nil))
             (pattern (if regexp query (regexp-quote query)))
             (case-fold-search case-fold)
             matches)
        (cl-loop for line in lines
                 for index from 0
                 while (< (length matches) max-results)
                 when (string-match-p pattern line)
                 do (let ((start (max 0 (- index context)))
                          (end (min (length lines) (+ index context 1))))
                      (push `((file . ,display-file)
                              (line . ,(1+ index))
                              (text . ,(mentat-text-search--bounded-line line))
                              ,@(when (> context 0)
                                  `((before . ,(mapcar
                                                #'mentat-text-search--bounded-line
                                                (seq-subseq lines start index)))
                                    (after . ,(mapcar
                                               #'mentat-text-search--bounded-line
                                               (seq-subseq lines (1+ index) end))))))
                            matches)))
        (nreverse matches)))))

(mentat-defun mentat-text-search-file
    (query file &key regexp (max-results 100) (context 0) case-fold)
  "Search FILE for QUERY and return compact grep-style matches.
When REGEXP is nil, treat QUERY literally.  CONTEXT controls how many lines
appear before and after each match.  CASE-FOLD enables case-insensitive search."
  (:execution sync :display "Search File")
  (let ((limit (min 500 (max 1 max-results)))
        (nearby (min 10 (max 0 context)))
        (absolute (expand-file-name file)))
    (mentat-text-search--format-matches
     (mentat-text-search--file
      query absolute regexp limit nearby case-fold file))))

(defconst mentat-text-search--rg-output-limit (* 4 1024 1024)
  "Maximum ripgrep JSON output retained for one project search.")

(defun mentat-text-search--rg-text (object)
  "Return the text payload from ripgrep JSON OBJECT."
  (and (listp object) (alist-get 'text object)))

(defun mentat-text-search--rg-relative-file (path)
  "Return normalized project-relative ripgrep PATH."
  (if (string-prefix-p "./" path) (substring path 2) path))

(defun mentat-text-search--parse-rg-events (lines limit nearby)
  "Convert ripgrep JSON LINES into at most LIMIT matches with NEARBY context."
  (let (before current matches)
    (cl-labels
        ((finish-current
          ()
          (when current
            (let ((entry
                   `((file . ,(plist-get current :file))
                     (line . ,(plist-get current :line))
                     (text . ,(plist-get current :text))
                     ,@(when (> nearby 0)
                         `((before . ,(plist-get current :before))
                           (after . ,(plist-get current :after)))))))
              (push entry matches)
              (setq current nil)))))
      (catch 'done
        (dolist (line lines)
          (unless (string-empty-p line)
            (let* ((event (json-parse-string
                           line :object-type 'alist :array-type 'list
                           :null-object nil :false-object nil))
                   (type (alist-get 'type event))
                   (data (alist-get 'data event))
                   (line-number (alist-get 'line_number data))
                   (text (mentat-text-search--rg-text
                          (alist-get 'lines data))))
              (pcase type
                ("begin"
                 (finish-current)
                 (setq before nil))
                ("context"
                 (when (stringp text)
                   (let ((bounded
                          (mentat-text-search--bounded-line
                           (string-remove-suffix "\n" text))))
                     (when (and current
                                (> line-number (plist-get current :line))
                                (<= (- line-number (plist-get current :line))
                                    nearby))
                       (setf (plist-get current :after)
                             (append (plist-get current :after)
                                     (list bounded))))
                     (setq before
                           (if (> nearby 0)
                               (last (append before
                                             (list (cons line-number bounded)))
                                     nearby)
                             nil)))))
                ("match"
                 (let ((bounded
                        (mentat-text-search--bounded-line
                         (string-remove-suffix "\n" text))))
                   (when (and current
                              (> line-number (plist-get current :line))
                              (<= (- line-number (plist-get current :line))
                                  nearby))
                     (setf (plist-get current :after)
                           (append (plist-get current :after)
                                   (list bounded))))
                   (finish-current)
                   (when (>= (length matches) limit)
                     (throw 'done nil))
                   (setq current
                         (list
                          :file (mentat-text-search--rg-relative-file
                                 (mentat-text-search--rg-text
                                  (alist-get 'path data)))
                          :line line-number
                          :text bounded
                          :before
                          (mapcar #'cdr
                                  (seq-filter
                                   (lambda (entry)
                                     (and (< (car entry) line-number)
                                          (<= (- line-number (car entry)) nearby)))
                                   before))
                          :after nil))
                   (setq before
                         (if (> nearby 0)
                             (last (append before
                                           (list (cons line-number bounded)))
                                   nearby)
                           nil))))
                ("end"
                 (finish-current)
                 (setq before nil)
                 (when (>= (length matches) limit)
                   (throw 'done nil)))))))
        (finish-current))
      (nreverse matches))))

(defun mentat-text-search--rg-command
    (query regexp glob limit nearby case-fold)
  "Build a ripgrep command for the supplied project-search arguments."
  (append
   (list (or (executable-find "rg")
             (user-error "ripgrep is required for project text search"))
         "--json" "--color" "never" "--sort" "path"
         "--hidden" "--glob" "!.git" "--max-count" (number-to-string limit)
         "--context" (number-to-string nearby))
   (unless regexp (list "--fixed-strings"))
   (when case-fold (list "--ignore-case"))
   (when glob (list "--glob" glob))
   (list "--" query ".")))

(mentat-defun mentat-text-search-project
    (query &key directory regexp glob (max-results 100) (context 0) case-fold)
  "Search project files for QUERY and return compact grep-style matches.
DIRECTORY defaults to the current project.  REGEXP selects regular-expression
matching; otherwise QUERY is literal.  GLOB limits relative file names using
ripgrep glob syntax.  CONTEXT adds up to ten surrounding lines."
  (:execution async :display "Project Grep")
  (let* ((base (expand-file-name (or directory default-directory)))
         (project (or (project-current nil base)
                      (user-error "No project found from %s" base)))
         (root (project-root project))
         (limit (min 500 (max 1 max-results)))
         (nearby (min 10 (max 0 context)))
         (command (mentat-text-search--rg-command
                   query regexp glob limit nearby case-fold)))
    (lambda (resolve reject on-cancel)
      (let ((output-lines nil)
            (pending "")
            (output-bytes 0)
            (match-count 0)
            (after-left nil)
            (completed nil)
            (stop-requested nil)
            (error-buffer
             (generate-new-buffer " *mentat-project-text-search-error*"))
            process)
        (cl-labels
            ((cleanup
              ()
              (when (buffer-live-p error-buffer)
                (kill-buffer error-buffer)))
             (reject-once
              (reason)
              (unless completed
                (setq completed t)
                (cleanup)
                (funcall reject reason)))
             (resolve-once
              ()
              (unless completed
                (setq completed t)
                (condition-case err
                    (let ((value
                           (mentat-text-search--format-matches
                            (mentat-text-search--parse-rg-events
                             (nreverse output-lines) limit nearby))))
                      (cleanup)
                      (funcall resolve value))
                  (error
                   (cleanup)
                   (funcall reject err)))))
             (stop-after-limit
              ()
              (setq stop-requested t))
             (consume-line
              (line)
              (push line output-lines)
              (let* ((event (json-parse-string
                             line :object-type 'alist :array-type 'list
                             :null-object nil :false-object nil))
                     (type (alist-get 'type event)))
                (cond
                 ((equal type "match")
                  (if (>= match-count limit)
                      (stop-after-limit)
                    (setq match-count (1+ match-count))
                    (when (= match-count limit)
                      (if (= nearby 0)
                          (stop-after-limit)
                        (setq after-left nearby)))))
                 ((and after-left (equal type "context"))
                  (setq after-left (1- after-left))
                  (when (= after-left 0) (stop-after-limit)))
                 ((and after-left (equal type "end"))
                  (stop-after-limit)))))
             (filter-output
              (_process chunk)
              (unless completed
                (condition-case err
                    (progn
                      (setq output-bytes
                            (+ output-bytes (string-bytes chunk)))
                      (if (> output-bytes mentat-text-search--rg-output-limit)
                          (progn
                            (reject-once
                             "ripgrep output exceeded the 4 MiB search limit")
                            (stop-after-limit))
                        (setq pending (concat pending chunk))
                        (let ((start 0))
                          (while (string-match "\n" pending start)
                            (consume-line
                             (substring pending start (match-beginning 0)))
                            (setq start (match-end 0)))
                          (setq pending (substring pending start)))))
                  (error
                   (reject-once err)
                   (stop-after-limit)))
                (when (and stop-requested (process-live-p process))
                  (delete-process process))))
             (finish-process
              (finished-process _event)
              (unless completed
                (when (not (string-empty-p pending))
                  (consume-line pending)
                  (setq pending ""))
                (let ((status (process-exit-status finished-process)))
                  (if (or (memq status '(0 1))
                          (>= match-count limit))
                      (resolve-once)
                    (let ((diagnostic
                           (when (buffer-live-p error-buffer)
                             (with-current-buffer error-buffer
                               (string-trim (buffer-string))))))
                      (reject-once
                       (if (string-empty-p diagnostic)
                           (format "ripgrep failed with exit status %s" status)
                         (format "ripgrep failed: %s" diagnostic)))))))))
          (condition-case err
              (setq process
                    (let ((default-directory root))
                      (make-process
                       :name "mentat-project-text-search"
                       :command command
                       :connection-type 'pipe
                       :coding '(utf-8-unix . utf-8-unix)
                       :noquery t
                       :buffer nil
                       :stderr error-buffer
                       :filter #'filter-output
                       :sentinel #'finish-process)))
            (error (reject-once err)))
          (funcall on-cancel
                   (lambda ()
                     (setq completed t)
                     (when (process-live-p process)
                       (delete-process process))
                     (cleanup))))))))

(provide 'project-text-search)
;;; project-text-search.el ends here
