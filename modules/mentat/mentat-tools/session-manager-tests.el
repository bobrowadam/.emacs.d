;;; session-manager-tests.el --- Session management regression tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Exercise history, handoff, and shutdown without launching Pi or sending prompts.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'mentat)

(load (expand-file-name "session-manager.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t t)

(ert-deftest mentat-session-send-waits-for-acknowledgment ()
  (dolist (live '(t nil))
    (ert-info ((format "Initially live: %S" live))
      (with-temp-buffer
        (let* ((entry (mentat--registry-entry-create :session-id "session-id"))
               (view (mentat--buffer-make :buffer (current-buffer) :status "idle"))
               (submissions 0)
               (resolutions 0)
               result failure cleanup ready accepted)
          (cl-letf (((symbol-function 'mentat--open-live-view)
                     (lambda (_id) (and live view)))
                    ((symbol-function 'mentat--open-entry-view)
                     (lambda (_entry on-ready _on-error)
                       (setq ready on-ready)))
                    ((symbol-function 'mentat--buffer-submit)
                     (lambda (_view _prompt _attachments on-accepted _on-error)
                       (cl-incf submissions)
                       (setq accepted on-accepted)
                       "request-id")))
            (funcall (mentat-session-manager--send-starter entry "test prompt")
                     (lambda (value)
                       (cl-incf resolutions)
                       (setq result value))
                     (lambda (reason) (setq failure reason))
                     (lambda (callback) (setq cleanup callback)))
            (should-not failure)
            (should-not result)
            (should (functionp cleanup))
            (unless live
              (should (= submissions 0))
              (should (functionp ready))
              (funcall ready view nil))
            (should (= submissions 1))
            (should (functionp accepted))
            (should-not result)
            (funcall accepted nil '((id . "request-id")))
            (should-not failure)
            (should (equal (alist-get 'session-id result) "session-id"))
            (should (equal (alist-get 'request-id result) "request-id"))
            (should (equal (alist-get 'resumed result) (not live)))
            (funcall accepted nil '((id . "request-id")))
            (should (= resolutions 1))
            (funcall cleanup)
            (should (buffer-live-p (current-buffer)))))))))

(ert-deftest mentat-session-send-rejects-startup-errors ()
  (dolist (stage '(lookup resume))
    (ert-info ((format "Failure stage: %S" stage))
      (let ((entry (mentat--registry-entry-create :session-id "session-id"))
            result failures cleanup)
        (cl-letf (((symbol-function 'mentat--open-live-view)
                   (lambda (_id)
                     (when (eq stage 'lookup)
                       (error "Mock startup failure"))
                     nil))
                  ((symbol-function 'mentat--open-entry-view)
                   (lambda (&rest _args)
                     (error "Mock startup failure"))))
          (funcall (mentat-session-manager--send-starter entry "test prompt")
                   (lambda (value) (setq result value))
                   (lambda (reason) (push reason failures))
                   (lambda (callback) (setq cleanup callback)))
          (should-not result)
          (should (equal failures '("Mock startup failure")))
          (should (functionp cleanup))
          (funcall cleanup)
          (should (equal failures '("Mock startup failure"))))))))

(defun mentat-session-test--await (starter)
  "Run STARTER with a bounded wait in batch tests."
  (let (done result failure cancel)
    (unwind-protect
        (progn
          (funcall starter
                   (lambda (value) (setq result value done t))
                   (lambda (reason) (setq failure reason done t))
                   (lambda (callback) (setq cancel callback)))
          (let ((deadline (+ (float-time) 5)))
            (while (and (not done) (< (float-time) deadline))
              (accept-process-output nil 0.01)))
          (should done)
          (when failure (error "%s" failure))
          result)
      (when cancel (funcall cancel)))))

(defmacro mentat-session-test--with-transcript (text &rest body)
  "Run BODY with a temporary transcript containing TEXT and an ENTRY."
  (declare (indent 1))
  `(let* ((file (make-temp-file "mentat-history-test-"))
          (entry (mentat--registry-entry-create
                  :session-id "history-test" :session-file file)))
     (unwind-protect
         (progn
           (with-temp-file file (insert ,text))
           (cl-letf (((symbol-function 'mentat--open-entry-view)
                      (lambda (&rest _) (ert-fail "History must not resume Pi"))))
             ,@body))
       (delete-file file))))

(ert-deftest mentat-session-history-reads-before-compaction-and-other-branches ()
  (mentat-session-test--with-transcript
      (concat
       "{\"type\":\"session\",\"id\":\"s\"}\n"
       "{\"type\":\"message\",\"id\":\"a\",\"parentId\":null,\"message\":{\"role\":\"user\",\"content\":[{\"type\":\"text\",\"text\":\"Before compaction שלום\\nneedle[0]\"}]}}\n"
       "{\"type\":\"compaction\",\"id\":\"b\",\"parentId\":\"a\",\"summary\":\"Condensed history\"}\n"
       "{\"type\":\"message\",\"id\":\"c\",\"parentId\":\"b\",\"message\":{\"role\":\"assistant\",\"content\":[{\"type\":\"toolCall\",\"name\":\"lookup\",\"arguments\":{\"keys\":[\"one\",\"two\"]}}]}}\n"
       "{\"type\":\"message\",\"id\":\"d\",\"parentId\":\"c\",\"message\":{\"role\":\"toolResult\",\"toolName\":\"lookup\",\"content\":[{\"type\":\"text\",\"text\":\"NEEDLE[0] tool result\"}]}}\n"
       "{\"type\":\"message\",\"id\":\"e\",\"parentId\":\"a\",\"message\":{\"role\":\"user\",\"content\":\"Other branch\"}}\n")
    (let* ((page (mentat-session-test--await
                  (mentat-session-manager--history-starter entry 2 0 20 50000)))
           (rows (alist-get 'records page)))
      (should (= (length rows) 5))
      (should (equal (alist-get 'text (car rows)) "Before compaction שלום\nneedle[0]"))
      (should (equal (alist-get 'text (nth 1 rows)) "Condensed history"))
      (should (string-match-p "one.*two" (alist-get 'text (nth 2 rows))))
      (should (equal (alist-get 'tool-name (nth 3 rows)) "lookup"))
      (should (equal (alist-get 'parent-id (nth 4 rows)) "a"))
      (should-not (alist-get 'next page)))
    (let* ((first (mentat-session-test--await
                   (mentat-session-manager--history-starter entry 1 0 1 50000 "needle[0]")))
           (row (car (alist-get 'records first)))
           (next (alist-get 'next first))
           (second (mentat-session-test--await
                    (mentat-session-manager--history-starter
                     entry (alist-get 'start-line next) 0 20 50000 "needle[0]"))))
      (should (= (alist-get 'line row) 2))
      (should (equal (alist-get 'role row) "user"))
      (should (= (alist-get 'line (car (alist-get 'records second))) 5))
      (should-not (alist-get 'next second)))))

(ert-deftest mentat-session-history-pages-long-unicode-record-without-loss ()
  (let ((text (concat (make-string 65535 ?x) "שלום\nend")))
    (mentat-session-test--with-transcript
        (concat (decode-coding-string
                 (json-serialize
                  `((type . "message") (message . ((role . "user") (content . ,text)))))
                 'utf-8)
                "\n")
      (let ((cursor '((start-line . 1) (start-column . 0))) parts)
        (while cursor
          (let ((page (mentat-session-test--await
                       (mentat-session-manager--history-starter
                        entry (alist-get 'start-line cursor)
                        (alist-get 'start-column cursor) 20 30000))))
            (dolist (row (alist-get 'records page))
              (push (alist-get 'text row) parts))
            (setq cursor (alist-get 'next page))))
        (should (equal (apply #'concat (nreverse parts)) text))))))

(ert-deftest mentat-session-history-handles-incomplete-tail-and-corruption ()
  (mentat-session-test--with-transcript
      "{\"type\":\"message\",\"message\":{\"role\":\"user\",\"content\":\"Complete\"}}\n{\"type\":"
    (let ((page (mentat-session-test--await
                 (mentat-session-manager--history-starter entry 1 0 20 20000))))
      (should (= (length (alist-get 'records page)) 1))
      (should (alist-get 'incomplete-tail page))
      (should (= (alist-get 'start-line (alist-get 'next page)) 2)))
    (with-temp-file file (insert "not JSON\n"))
    (should-error
     (mentat-session-test--await
      (mentat-session-manager--history-starter entry 1 0 20 20000)))))

(ert-deftest mentat-session-history-omits-images-and-rejects-missing-files ()
  (should (equal
           (mentat-session-manager--record-text
            '((type . "message")
              (message . ((content . [((type . "image") (data . "PRIVATE-IMAGE"))])))))
           "[image payload omitted]"))
  (let ((entry (mentat--registry-entry-create :session-id "missing")))
    (should-error (mentat-session-manager--history-file entry) :type 'user-error)))

(ert-deftest mentat-session-history-cancellation-does-not-deliver-results ()
  (mentat-session-test--with-transcript "{\"type\":\"session\"}\n"
    (let (cancel delivered)
      (funcall (mentat-session-manager--history-starter entry 1 0 20 20000)
               (lambda (_) (setq delivered t))
               (lambda (_) (setq delivered t))
               (lambda (callback) (setq cancel callback)))
      (funcall cancel)
      (sleep-for 0.02)
      (should-not delivered)
      (should-not (get-buffer " *mentat-history-read*")))))

(ert-deftest mentat-session-kill-refuses-active-or-displayed-and-preserves-history ()
  (mentat-session-test--with-transcript "{\"type\":\"session\"}\n"
    (let* ((buffer (generate-new-buffer " *mentat-kill-test*"))
           (view (mentat--buffer-make :buffer buffer :status "running"))
           (displayed nil)
           (forgotten nil))
      (unwind-protect
          (cl-letf (((symbol-function 'mentat--open-live-view) (lambda (_) view))
                    ((symbol-function 'get-buffer-window)
                     (lambda (&rest _) displayed))
                    ((symbol-function 'mentat--registry-forget)
                     (lambda (_) (setq forgotten t))))
            (should-error (mentat-session-manager--kill entry) :type 'user-error)
            (setf (mentat--buffer-status view) "idle"
                  displayed t)
            (should-error (mentat-session-manager--kill entry) :type 'user-error)
            (should-not forgotten)
            (should (buffer-live-p buffer))
            (setq displayed nil)
            (let ((result (mentat-session-manager--kill entry)))
              (should (equal (alist-get 'session-id result) "history-test"))
              (should (alist-get 'pi-transcript-preserved result))
              (should forgotten)
              (should-not (buffer-live-p buffer))
              (should (file-exists-p file))))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest mentat-session-close-preserves-history-and-waits-for-process-exit ()
  (mentat-session-test--with-transcript "{\"type\":\"session\"}\n"
    (let* ((buffer (generate-new-buffer " *mentat-close-test*"))
           (process (make-process :name "mentat-close-test" :command '("cat")
                                  :connection-type 'pipe :buffer nil :noquery t))
           (connection (mentat--rpc-make-connection :process process))
           (session (mentat--session-make :connection connection))
           (view (mentat--buffer-make :buffer buffer :session session :status "idle")))
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (setq-local mentat--buffer-view view)
              (add-hook 'kill-buffer-hook #'mentat--buffer-stop-session nil t))
            (cl-letf (((symbol-function 'mentat--open-live-view) (lambda (_) view))
                      ((symbol-function 'mentat--buffer-cancel-renderer-resources) #'ignore))
              (let ((result (mentat-session-test--await
                             (mentat-session-manager--close-starter entry nil))))
                (should (equal (alist-get 'status result) "closed"))
                (should (alist-get 'history-preserved result))
                (should-not (buffer-live-p buffer))
                (should-not (process-live-p process))
                (should (file-exists-p file)))))
        (when (buffer-live-p buffer) (kill-buffer buffer))
        (when (process-live-p process) (delete-process process))))))

(ert-deftest mentat-session-close-refuses-busy-and-displayed-sessions ()
  (dolist (state '("running" "starting" "idle"))
    (with-temp-buffer
      (let* ((entry (mentat--registry-entry-create :session-id "test"))
             (session (mentat--session-make))
             (view (mentat--buffer-make :buffer (current-buffer) :session session :status state)))
        (cl-letf (((symbol-function 'mentat--open-live-view) (lambda (_) view))
                  ((symbol-function 'get-buffer-window)
                   (lambda (&rest _) (equal state "idle"))))
          (should-error (mentat-session-test--await
                         (mentat-session-manager--close-starter entry nil)))
          (should (buffer-live-p (current-buffer))))))))

(ert-deftest mentat-session-close-aborts-only-with-explicit-interrupt ()
  (let* ((buffer (generate-new-buffer " *mentat-close-active-test*"))
         (entry (mentat--registry-entry-create :session-id "test"))
         (session (mentat--session-make :active-prompt "prompt"))
         (view (mentat--buffer-make :buffer buffer :session session :status "running"))
         callback clear-callback cancel result failure)
    (unwind-protect
        (cl-letf (((symbol-function 'mentat--open-live-view) (lambda (_) view))
                  ((symbol-function 'mentat--session-clear-queue)
                   (lambda (_session on-response) (setq clear-callback on-response)))
                  ((symbol-function 'mentat--session-abort)
                   (lambda (_session on-response) (setq callback on-response))))
          (should-error (mentat-session-test--await
                         (mentat-session-manager--close-starter entry nil)))
          (should-not callback)
          (funcall (mentat-session-manager--close-starter entry t)
                   (lambda (value) (setq result value))
                   (lambda (reason) (setq failure reason))
                   (lambda (fn) (setq cancel fn)))
          (should clear-callback)
          (should-not callback)
          (funcall clear-callback '((success . t)))
          (should callback)
          (should-not result)
          (should (buffer-live-p buffer))
          (funcall callback '((success . t)))
          (should-not failure)
          (should (equal (alist-get 'status result) "closed"))
          (should-not (buffer-live-p buffer))
          (funcall cancel))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest mentat-session-close-already-closed-is-a-no-op ()
  (cl-letf (((symbol-function 'mentat--open-live-view) (lambda (_) nil)))
    (let ((result (mentat-session-test--await
                   (mentat-session-manager--close-starter
                    (mentat--registry-entry-create :session-id "closed") nil))))
      (should (equal (alist-get 'status result) "already-closed")))))

(ert-deftest mentat-session-history-searches-non-content-fields-without-images ()
  (mentat-session-test--with-transcript
      (concat
       (mapconcat
        (lambda (record) (decode-coding-string (json-serialize record) 'utf-8))
        '(((type . "message")
           (message . ((role . "bashExecution")
                       (command . "echo שלום") (output . "shell output"))))
          ((type . "message")
           (message . ((role . "system") (content . "")
                       (sections . ((rules . "Section instructions"))))))
          ((type . "message")
           (message . ((role . "assistant") (content . [])
                       (errorMessage . "Provider rejected"))))
          ((type . "custom_message")
           (content . [((type . "text") (text . "extension note"))
                       ((type . "image") (data . "PRIVATE-IMAGE"))]))
          ((type . "message")
           (message . ((role . "branchSummary") (summary . "branch summary")))))
        "\n")
       "\n")
    (dolist (query '("echo שלום" "shell output" "Section instructions"
                     "Provider rejected" "extension note" "branch summary"))
      (let ((page (mentat-session-test--await
                   (mentat-session-manager--history-starter entry 1 0 20 20000 query))))
        (should (= (length (alist-get 'records page)) 1))))
    (let ((page (mentat-session-test--await
                 (mentat-session-manager--history-starter entry 1 0 20 20000))))
      (should-not (string-match-p "PRIVATE-IMAGE" (format "%S" page))))))

(ert-deftest mentat-session-close-keeps-buffer-on-failure-or-cancel ()
  (dolist (stage '(clear abort cancel))
    (let* ((buffer (generate-new-buffer " *mentat-close-rejection-test*"))
           (entry (mentat--registry-entry-create :session-id "test"))
           (session (mentat--session-make :active-prompt "prompt"))
           (view (mentat--buffer-make :buffer buffer :session session :status "running"))
           clear-callback abort-callback cancel result failure)
      (unwind-protect
          (cl-letf (((symbol-function 'mentat--open-live-view) (lambda (_) view))
                    ((symbol-function 'mentat--session-clear-queue)
                     (lambda (_session callback) (setq clear-callback callback)))
                    ((symbol-function 'mentat--session-abort)
                     (lambda (_session callback) (setq abort-callback callback))))
            (funcall (mentat-session-manager--close-starter entry t)
                     (lambda (value) (setq result value))
                     (lambda (reason) (setq failure reason))
                     (lambda (callback) (setq cancel callback)))
            (pcase stage
              ('clear (funcall clear-callback '((success . :false) (error . "clear failed"))))
              ('abort
               (funcall clear-callback '((success . t)))
               (funcall abort-callback '((success . :false) (error . "abort failed"))))
              ('cancel
               (funcall cancel)
               (funcall clear-callback '((success . t)))
               (should-not abort-callback)))
            (should (eq (and failure t) (not (eq stage 'cancel))))
            (should-not result)
            (should (buffer-live-p buffer)))
        (when cancel (funcall cancel))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest mentat-session-close-allows-inactive-failed-sessions ()
  (dolist (status '("failed" "error: connection lost"))
    (let* ((buffer (generate-new-buffer " *mentat-close-failed-test*"))
           (entry (mentat--registry-entry-create :session-id "test"))
           (session (mentat--session-make))
           (view (mentat--buffer-make :buffer buffer :session session :status status)))
      (unwind-protect
          (cl-letf (((symbol-function 'mentat--open-live-view) (lambda (_) view)))
            (let ((result (mentat-session-test--await
                           (mentat-session-manager--close-starter entry nil))))
              (should (equal (alist-get 'status result) "closed"))
              (should-not (buffer-live-p buffer))))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(provide 'session-manager-tests)
;;; session-manager-tests.el ends here
