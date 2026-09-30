;;; session-manager.el --- Shared Mentat session support -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'json)
(require 'mentat-session)
(require 'mentat-rpc)

(require 'subr-x)

(require 'mentat-buffer)

(require 'mentat-prompt)

(require 'mentat-registry)

(require 'mentat-ui)

(require 'seq)

(defun mentat-session-manager--result (buffer state directory request-id)
  "Return bounded session metadata for BUFFER and Pi STATE in DIRECTORY.
REQUEST-ID identifies the optional initial handoff submission."
  (let ((result
         `((session-id . ,(alist-get 'sessionId state))
           (buffer . ,(buffer-name buffer))
           (directory . ,directory))))
    (if request-id
        (append result `((handoff-request-id . ,request-id)))
      result)))

(defun mentat-session-manager--starter (directory name handoff)
  "Return a callback starter creating a Mentat session in DIRECTORY.
NAME optionally names the session.  HANDOFF is submitted once Pi is ready."
  (lambda (resolve reject on-cancel)
    (let ((root (file-name-as-directory (file-truename directory)))
          buffer
          settled)
      (cl-labels
          ((dispose ()
             (when (buffer-live-p buffer)
               (kill-buffer buffer)))
           (succeed (value)
             (unless settled
               (setq settled t)
               (funcall resolve value)))
           (fail (reason)
             (unless settled
               (setq settled t)
               (dispose)
               (funcall reject reason)))
           (cancel ()
             (unless settled
               (setq settled t)
               (dispose))))
        (funcall on-cancel #'cancel)
        (condition-case err
            (setq buffer
                  (mentat-start-session
                   root
                   :name name
                   :ready-handler
                   (lambda (ready-buffer state)
                     (setq buffer ready-buffer)
                     (if (and handoff (not (string-blank-p handoff)))
                         (with-current-buffer ready-buffer
                           (if-let* ((request-id (mentat-submit handoff)))
                               (succeed
                                (mentat-session-manager--result
                                 ready-buffer state root request-id))
                             (fail
                              "Mentat session started but the handoff could not be submitted")))
                       (succeed
                        (mentat-session-manager--result
                         ready-buffer state root nil))))
                   :error-handler
                   (lambda (failed-buffer response)
                     (setq buffer failed-buffer)
                     (fail (or (alist-get 'error response)
                               "Mentat session startup failed")))))
          (error
           (fail (error-message-string err))))))))

(defun mentat-session-manager--reload-starter (buffer)
  "Return a callback starter that reloads the Mentat session in BUFFER."
  (lambda (resolve reject on-cancel)
    (let (settled)
      (cl-labels
          ((succeed (value)
             (unless settled
               (setq settled t)
               (funcall resolve value)))
           (fail (reason)
             (unless settled
               (setq settled t)
               (funcall reject reason)))
           (cancel ()
             (setq settled t)))
        (funcall on-cancel #'cancel)
        (condition-case err
            (with-current-buffer buffer
              (mentat-reload
               (lambda (ready-buffer state)
                 (succeed
                  (mentat-session-manager--result
                   ready-buffer state
                   (file-name-as-directory
                    (file-truename
                     (buffer-local-value 'default-directory ready-buffer)))
                   nil)))
               (lambda (_failed-buffer reason)
                 (fail reason))))
          (error
           (fail (error-message-string err))))))))

(defun mentat-session-manager--check-killable (entry)
  "Refuse to kill a displayed or busy registered session ENTRY."
  (when-let* ((view (mentat--open-live-view
                    (mentat--registry-entry-session-id entry))))
    (let ((buffer (mentat--buffer-buffer view)))
      (when (get-buffer-window buffer t)
        (user-error "Session is displayed; hide its buffer before killing"))
      (unless (member (mentat--buffer-status view) '("idle" "closed"))
        (user-error "Session is busy; close it before killing")))))

(defun mentat-session-manager--kill (entry)
  "Forget registered session ENTRY and close its buffer, preserving Pi history."
  (mentat-session-manager--check-killable entry)
  (let* ((session-id (mentat--registry-entry-session-id entry))
         (view (mentat--open-live-view session-id))
         (buffer (and view (mentat--buffer-buffer view))))
    (unless (mentat--registry-forget session-id)
      (user-error "Mentat session is no longer registered: %s" session-id))
    (when (buffer-live-p buffer)
      (unless (kill-buffer buffer)
        (user-error "Session registration was removed but its buffer refused to close")))
    `((session-id . ,session-id) (status . "killed")
      (pi-transcript-preserved . t))))

(defconst mentat-session-manager--list-limit 100
  "Maximum sessions returned by `mentat-session-list'.")

(defun mentat-session-manager--entry-result (entry)
  "Return bounded metadata for registered session ENTRY."
  (let* ((session-id (mentat--registry-entry-session-id entry))
         (metadata (mentat--open-session-file-metadata
                    (mentat--registry-entry-session-file entry)))
         (view (mentat--open-live-view session-id))
         (buffer (and view (mentat--buffer-buffer view)))
         (modified (mentat--open-entry-modified-time entry))
         (last-activity
          (and modified
               (format-time-string "%Y-%m-%dT%H:%M:%SZ" modified t))))
    `((session-id . ,session-id)
      (name . ,(or (plist-get metadata :name)
                    (plist-get metadata :preview)
                    "Unnamed session"))
      (directory . ,(mentat--registry-entry-root entry))
      (status . ,(if view (mentat--buffer-status view) "closed"))
      (live . ,(and view t))
      (last-activity . ,last-activity)
      ,@(when (buffer-live-p buffer)
          `((buffer . ,(buffer-name buffer)))))))

(defun mentat-session-manager--search-starter (entries query limit unavailable)
  "Return a callback starter searching ENTRIES for QUERY, bounded by LIMIT.
UNAVAILABLE counts registered sessions without a readable transcript."
  (lambda (resolve reject on-cancel)
    (let ((output "") (finished nil) process stderr)
      (cl-labels
          ((cleanup ()
             (when (and process (process-live-p process))
               (delete-process process))
             (when (buffer-live-p stderr) (kill-buffer stderr)))
           (fail (reason)
             (unless finished
               (setq finished t)
               (cleanup)
               (funcall reject reason)))
           (succeed ()
             (unless finished
               (setq finished t)
               (let* ((files (split-string output "\0" t))
                      (hits (make-hash-table :test #'equal))
                      matches)
                 (dolist (file files) (puthash file t hits))
                 (dolist (entry entries)
                   (when (gethash (mentat--registry-entry-session-file entry) hits)
                     (push entry matches)))
                 (setq matches (nreverse matches))
                 (let ((result
                        `((searched . ,(length entries))
                          (unavailable . ,unavailable)
                          (truncated . ,(> (length matches) limit))
                          (sessions . ,(mapcar
                                        (lambda (entry)
                                          (append
                                           (mentat-session-manager--entry-result entry)
                                           `((session-file . ,(mentat--registry-entry-session-file entry)))))
                                        (seq-take matches limit))))))
                   (cleanup)
                   (funcall resolve result)))))
           (filter-output (_ chunk)
             (unless finished
               (setq output (concat output chunk))
               (when (> (length output) 131072)
                 (fail "Session search output exceeded 128 KiB"))))
           (on-exit (proc _event)
             (unless finished
               (if (memq (process-exit-status proc) '(0 1))
                   (succeed)
                 (fail (format "Session search failed: %s"
                               (if (buffer-live-p stderr)
                                   (string-trim
                                    (with-current-buffer stderr
                                      (buffer-substring-no-properties
                                       (point-min) (min (point-max) 1024))))
                                 (process-exit-status proc))))))))
        (funcall on-cancel
                 (lambda ()
                   (unless finished
                     (setq finished t)
                     (cleanup))))
        (condition-case err
            (if (null entries)
                (succeed)
              (setq stderr (generate-new-buffer " *mentat-session-search-error*"))
              (setq process
                    (make-process
                     :name "mentat-session-search" :buffer nil :stderr stderr
                     :command (append
                               (list (or (executable-find "rg")
                                         (user-error "ripgrep is required"))
                                     "--files-with-matches" "--null" "--fixed-strings"
                                     "--ignore-case" "--" query)
                               (mapcar #'mentat--registry-entry-session-file entries))
                     :connection-type 'pipe :coding '(utf-8-unix . utf-8-unix)
                     :noquery t :filter #'filter-output :sentinel #'on-exit)))
          (error (fail (error-message-string err))))))))

(defun mentat-session-manager--send-starter (entry prompt)
  "Return a callback starter that sends PROMPT to registered session ENTRY."
  (lambda (resolve reject on-cancel)
    (let ((session-id (mentat--registry-entry-session-id entry))
          buffer
          resumed
          submitted
          settled)
      (cl-labels
          ((succeed (value)
             (unless settled
               (setq settled t)
               (funcall resolve value)))
           (fail (reason)
             (unless settled
               (setq settled t)
               (funcall reject reason)))
           (cancel ()
             (unless settled
               (setq settled t)
               (when (and resumed (not submitted) (buffer-live-p buffer))
                 (kill-buffer buffer))))
           (send (view)
             (setq buffer (mentat--buffer-buffer view))
             (if (not (equal "idle" (mentat--buffer-status view)))
                 (fail (format "Mentat session is not idle: %s" session-id))
               (condition-case err
                   (let ((request-id
                          (mentat--buffer-submit
                           view prompt nil
                           (lambda (_session response)
                             (succeed
                              `((session-id . ,session-id)
                                (request-id . ,(alist-get 'id response))
                                (buffer . ,(buffer-name buffer))
                                (resumed . ,resumed))))
                           (lambda (_session response)
                             (fail (or (alist-get 'error response)
                                       "Mentat prompt was rejected"))))))
                     (setq submitted (and request-id t)))
                 (error (fail (error-message-string err)))))))
        (funcall on-cancel #'cancel)
        (condition-case err
            (if-let* ((view (mentat--open-live-view session-id)))
                (send view)
              (setq resumed t)
              (mentat--open-entry-view
               entry
               (lambda (view _state) (send view))
               (lambda (_view response)
                 (fail (or (alist-get 'error response)
                           "Mentat session resume failed")))))
          (error (fail (error-message-string err))))))))

(defun mentat-session-manager--find-entry (session-id)
  "Find registered SESSION-ID without opening or resuming it."
  (unless (and (stringp session-id) (not (string-blank-p session-id)))
    (user-error "SESSION-ID must be a nonblank string"))
  (or (seq-find (lambda (entry)
                  (equal session-id (mentat--registry-entry-session-id entry)))
                (mentat--registry-list))
      (user-error "No registered Mentat session: %s" session-id)))

(defun mentat-session-manager--history-file (entry)
  "Return ENTRY's readable local transcript, or report its absence."
  (let ((file (mentat--registry-entry-session-file entry)))
    (unless (and (stringp file) (not (file-remote-p file))
                 (file-regular-p file) (file-readable-p file))
      (user-error "Session transcript is unavailable: %s"
                  (mentat--registry-entry-session-id entry)))
    file))

(defun mentat-session-manager--content-text (content)
  "Render Pi CONTENT blocks as text without image or replay payloads."
  (if (stringp content) content
    (mapconcat
     (lambda (part)
       (pcase (alist-get 'type part)
         ("text" (or (alist-get 'text part) ""))
         ("thinking" (or (alist-get 'thinking part) ""))
         ("image" "[image payload omitted]")
         ("toolCall"
          (format "[tool call: %s]\n%s" (alist-get 'name part)
                  (decode-coding-string
                   (json-serialize (alist-get 'arguments part)) 'utf-8)))
         (_ (decode-coding-string (json-serialize part) 'utf-8))))
     content "\n\n")))

(defun mentat-session-manager--record-text (record)
  "Return searchable text for a persisted Pi RECORD, omitting image payloads."
  (pcase (alist-get 'type record)
    ("message"
     (let ((message (alist-get 'message record)))
       (string-join
        (seq-filter
         (lambda (text) (and text (not (string-empty-p text))))
         (list
          (mentat-session-manager--content-text (alist-get 'content message))
          (when (equal (alist-get 'role message) "bashExecution")
            (format "[command]\n%s\n[output]\n%s"
                    (or (alist-get 'command message) "")
                    (or (alist-get 'output message) "")))
          (when (alist-get 'sections message)
            (decode-coding-string (json-serialize (alist-get 'sections message)) 'utf-8))
          (alist-get 'errorMessage message)
          (alist-get 'summary message)))
        "\n\n")))
    ("custom_message"
     (mentat-session-manager--content-text (alist-get 'content record)))
    ((or "compaction" "branch_summary") (or (alist-get 'summary record) ""))
    (_ (decode-coding-string (json-serialize record) 'utf-8))))

(defun mentat-session-manager--scan-history (file start-line visit)
  "Return an async starter scanning FILE from START-LINE.
Call VISIT with each line number and decoded record until it returns nil.
Read a fixed-size snapshot in small timer chunks, without opening a session."
  (lambda (resolve reject on-cancel)
    (let ((pending (generate-new-buffer " *mentat-history-read*"))
          (offset 0) (line 1) size timer settled)
      (with-current-buffer pending (set-buffer-multibyte nil))
      (cl-labels
          ((cleanup ()
             (when timer (cancel-timer timer))
             (when (buffer-live-p pending) (kill-buffer pending)))
           (finish (incomplete)
             (unless settled
               (setq settled t)
               (cleanup)
               (funcall resolve `((next-line . ,line)
                                  (incomplete-tail . ,incomplete)))))
           (fail (reason)
             (unless settled
               (setq settled t)
               (cleanup)
               (funcall reject reason)))
           (step ()
             (unless settled
               (condition-case err
                   (with-current-buffer pending
                     (goto-char (point-max))
                     (let ((end (min size (+ offset 65536))))
                       (insert-file-contents-literally file nil offset end)
                       (setq offset end))
                     (goto-char (point-min))
                     (while (and (not settled) (search-forward "\n" nil t))
                       (when (> (- (point) (point-min) 1) (* 16 1024 1024))
                         (error "Transcript record at line %d exceeds 16 MiB" line))
                       (let* ((end (point))
                              (keep-going
                               (or (< line start-line)
                                   (funcall
                                    visit line
                                    (condition-case nil
                                        (json-parse-string
                                         (decode-coding-string
                                          (buffer-substring-no-properties
                                           (point-min) (1- end)) 'utf-8)
                                         :object-type 'alist :array-type 'array)
                                      (json-parse-error
                                       (error "Invalid JSON in transcript at line %d" line)))))))
                         (delete-region (point-min) end)
                         (cl-incf line)
                         (unless keep-going (finish nil))))
                     (unless settled
                       (when (> (buffer-size) (* 16 1024 1024))
                         (error "Transcript record at line %d exceeds 16 MiB" line))
                       (if (= offset size)
                           (finish (> (buffer-size) 0))
                         (setq timer (run-at-time 0.001 nil #'step)))))
                 (error (fail (error-message-string err)))))))
        (funcall on-cancel (lambda () (setq settled t) (cleanup)))
        (condition-case err
            (progn
              (setq size (file-attribute-size (file-attributes file)))
              (setq timer (run-at-time 0 nil #'step)))
          (error (fail (error-message-string err))))))))

(defun mentat-session-manager--history-starter
    (entry start-line start-column max-records max-chars &optional query)
  "Return a paged history reader or literal QUERY search for ENTRY.
START-COLUMN is a zero-based offset into rendered record text, not raw JSON."
  (let ((file (mentat-session-manager--history-file entry)))
    (lambda (resolve reject on-cancel)
      (let ((rows nil) (used 0) next)
        (funcall
         (mentat-session-manager--scan-history
          file start-line
          (lambda (line record)
            (let* ((text (mentat-session-manager--record-text record))
                   (column (if (= line start-line) start-column 0))
                   (case-fold-search t))
              (when (> column (length text))
                (user-error "START-COLUMN exceeds text length at line %d" line))
              (let ((hit (and query (string-match (regexp-quote query) text column))))
                (when (or (not query) hit)
                  (let* ((start (if query (max 0 (- hit 200)) column))
                         (end (min (length text)
                                   (if query (+ hit (length query) 200)
                                     (+ start (- max-chars used)))))
                         (message (alist-get 'message record)))
                    (push `((line . ,line)
                            (id . ,(alist-get 'id record))
                            (parent-id . ,(alist-get 'parentId record))
                            (type . ,(alist-get 'type record))
                            (timestamp . ,(alist-get 'timestamp record))
                            (role . ,(alist-get 'role message))
                            (tool-name . ,(alist-get 'toolName message))
                            (start-column . ,start)
                            (text-length . ,(length text))
                            ,@(when query `((match-column . ,hit)))
                            (text . ,(substring text start end)))
                          rows)
                    (cl-incf used (- end start))
                    (when (or (and (not query) (< end (length text)))
                              (>= (length rows) max-records)
                              (and (not query) (>= used max-chars)))
                      (setq next
                            (if (and (not query) (< end (length text)))
                                `((start-line . ,line) (start-column . ,end))
                              `((start-line . ,(1+ line)) (start-column . 0))))))))
              (not next))))
         (lambda (scan)
           (let ((cursor (or next
                             (when (alist-get 'incomplete-tail scan)
                               `((start-line . ,(alist-get 'next-line scan))
                                 (start-column . 0))))))
             (funcall resolve
                      `((session-id . ,(mentat--registry-entry-session-id entry))
                        (session-file . ,file)
                        (scope . "persisted records across all branches")
                        (records . ,(nreverse rows))
                        (next . ,cursor)
                        (incomplete-tail . ,(alist-get 'incomplete-tail scan))))))
         reject on-cancel)))))

(defun mentat-session-manager--close-starter (entry interrupt)
  "Return a starter closing ENTRY, allowing active work only with INTERRUPT.
Use normal buffer shutdown hooks and wait for the Pi process to exit."
  (lambda (resolve reject on-cancel)
    (let ((session-id (mentat--registry-entry-session-id entry))
          timer settled)
      (cl-labels
          ((finish (result)
             (unless settled
               (setq settled t)
               (when timer (cancel-timer timer))
               (funcall resolve result)))
           (fail (reason)
             (unless settled
               (setq settled t)
               (when timer (cancel-timer timer))
               (funcall reject reason)))
           (wait-for-exit (process)
             (unless settled
               (if (and process (process-live-p process))
                   (setq timer (run-at-time 0.05 nil #'wait-for-exit process))
                 (finish `((session-id . ,session-id) (status . "closed")
                           (history-preserved . t))))))
           (close-view (view process)
             (unless settled
               (condition-case err
                   (let ((buffer (mentat--buffer-buffer view)))
                     (when (get-buffer-window buffer t)
                       (user-error "Session is displayed; hide its buffer before closing"))
                     (when (buffer-live-p buffer)
                       (unless (kill-buffer buffer)
                         (user-error "Session buffer refused to close")))
                     (wait-for-exit process))
                 (error (fail (error-message-string err))))))
           (abort-view (view session process)
             (unless settled
               (condition-case err
                   (if (mentat--session-active-prompt session)
                       (mentat--session-abort
                        session
                        (lambda (response)
                          (unless settled
                            (if (eq t (alist-get 'success response))
                                (close-view view process)
                              (fail (or (alist-get 'error response)
                                        "Session abort failed"))))))
                     (close-view view process))
                 (error (fail (error-message-string err)))))))
        (funcall on-cancel
                 (lambda ()
                   (setq settled t)
                   (when timer (cancel-timer timer))))
        (condition-case err
            (if-let* ((view (mentat--open-live-view session-id)))
                (let* ((buffer (mentat--buffer-buffer view))
                       (session (mentat--buffer-session view))
                       (connection (mentat--session-connection session))
                       (process (and connection
                                     (mentat--rpc-connection-process connection)))
                       (active (mentat--session-active-prompt session))
                       (busy (or active
                                 (not (or (member (mentat--buffer-status view)
                                                  '("idle" "closed"))
                                          (mentat--open-failed-status-p
                                           (mentat--buffer-status view)))))))
                  (when (get-buffer-window buffer t)
                    (user-error "Session is displayed; hide its buffer before closing"))
                  (when (and busy (not interrupt))
                    (user-error "Session is busy; explicit approval and INTERRUPT=true are required"))
                  (if active
                      (mentat--session-clear-queue
                       session
                       (lambda (response)
                         (unless settled
                           (if (eq t (alist-get 'success response))
                               (abort-view view session process)
                             (fail (or (alist-get 'error response)
                                       "Clearing session queues failed"))))))
                    (close-view view process)))
              (finish `((session-id . ,session-id) (status . "already-closed")
                        (history-preserved . t))))
          (error (fail (error-message-string err))))))))

(provide 'session-manager)
;;; session-manager.el ends here
