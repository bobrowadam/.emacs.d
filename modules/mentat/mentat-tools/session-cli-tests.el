;;; session-cli-tests.el --- Session CLI regression tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Run only in isolated batch Emacs.  No real sessions or model calls are made.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)
(require 'server)
(require 'mentat)

(load (expand-file-name "session-cli.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t t)
(bob/mentat-session-cli-starter
 "{\"command\":\"list\",\"arguments\":[],\"keywords\":{}}")

(defconst bob/session-cli-test-program
  (expand-file-name "~/dotfiles/mentat-session/mentat-session.py"))

(defun bob/session-cli-test-run (&rest arguments)
  "Run the real CLI with ARGUMENTS against this isolated Emacs server."
  (let ((output (generate-new-buffer " *session-cli-test-output*")) process)
    (unwind-protect
        (progn
          (setq process
                (make-process
                 :name "session-cli-test" :buffer output :noquery t
                 :connection-type 'pipe :coding 'utf-8-unix
                 :sentinel #'ignore
                 :command (append
                           (list "python3" bob/session-cli-test-program
                                 "--socket-name" (expand-file-name server-name server-socket-dir))
                           arguments)))
          (let ((deadline (+ (float-time) 10)))
            (while (and (process-live-p process) (< (float-time) deadline))
              (accept-process-output nil 0.01)))
          (should-not (process-live-p process))
          (list (process-exit-status process)
                (with-current-buffer output (buffer-string))))
      (when (and process (process-live-p process)) (delete-process process))
      (kill-buffer output))))

(defmacro bob/session-cli-test-with-server (&rest body)
  "Run BODY with a disposable batch-only server, registry, and project root."
  (declare (indent 0))
  `(progn
     (skip-unless noninteractive)
     (let* ((directory (make-temp-file "mentat-session-cli-test-" t))
            (default-directory (file-name-as-directory directory))
            (server-name "session-cli-test")
            (server-socket-dir directory)
            (server-use-tcp nil)
            (server-process nil)
            (server-clients nil)
            (mentat-registry-file (expand-file-name "sessions.eld" directory)))
       (unwind-protect
           (progn (server-start) ,@body)
         (when (and server-process (process-live-p server-process))
           (delete-process server-process))
         (delete-directory directory t)))))

(ert-deftest bob/session-cli-create-selects-basic-and-waits-for-handoff ()
  (bob/session-cli-test-with-server
    (let* ((mentat-pi-directory "/wrong/default")
           (mentat-pi-profiles `(("Work" :directory "/wrong/work")
                                ("basic" :directory ,default-directory
                                 :agent-features nil :extensions nil)))
           (name "Quotes \" and Unicode שלום")
           (handoff "Literal \" ) (error \"not code\")\nNew line")
           (buffer (generate-new-buffer " *session-cli-created*"))
           created-profile created-name sent-prompt)
      (unwind-protect
          (cl-letf (((symbol-function 'mentat-start-session)
                     (lambda (_root &rest options)
                       (setq created-profile mentat-pi-directory
                             created-name (plist-get options :name))
                       (run-at-time 0.01 nil (plist-get options :ready-handler)
                                    buffer '((sessionId . "created-id")))
                       buffer))
                    ((symbol-function 'mentat-submit)
                     (lambda (prompt &rest _)
                       (should (eq (current-buffer) buffer))
                       (setq sent-prompt prompt)
                       "handoff-request")))
            (pcase-let* ((`(,exit ,output)
                          (bob/session-cli-test-run
                           "create" default-directory "--name" name "--handoff" handoff))
                         (result (json-parse-string output :object-type 'alist)))
              (should (= exit 0))
              (should (equal (alist-get 'session-id result) "created-id"))
              (should (equal (alist-get 'handoff-request-id result) "handoff-request"))
              (should (equal created-profile (directory-file-name default-directory)))
              (should (equal created-name name))
              (should (equal sent-prompt handoff))
              (should (equal mentat-pi-directory "/wrong/default"))))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest bob/session-cli-history-streams-complete-json-without-resuming ()
  (bob/session-cli-test-with-server
    (let* ((file (expand-file-name "history.jsonl" default-directory))
           (text (concat (make-string 40000 ?x) "שלום\nneedle[0]"))
           (entry (mentat--registry-entry-create
                   :session-id "history" :session-file file :root default-directory)))
      (with-temp-file file
        (insert (json-encode `((type . "message") (id . "old") (parentId . nil)
                               (message . ((role . "user") (content . ,text))))) "\n"))
      (cl-letf (((symbol-function 'mentat--registry-list) (lambda (&rest _) (list entry)))
                ((symbol-function 'mentat--open-live-view) #'ignore)
                ((symbol-function 'mentat--open-entry-view)
                 (lambda (&rest _) (ert-fail "History must not resume a session"))))
        (pcase-let* ((`(,exit ,output)
                      (bob/session-cli-test-run "read" "history" "--max-chars" "50000"))
                     (result (json-parse-string output :object-type 'alist :array-type 'list)))
          (should (= exit 0))
          (should (equal (alist-get 'text (car (alist-get 'records result))) text)))
        (pcase-let* ((`(,exit ,output)
                      (bob/session-cli-test-run "search-history" "history" "NEEDLE[0]"))
                     (result (json-parse-string output :object-type 'alist :array-type 'list)))
          (should (= exit 0))
          (should (string-match-p "needle\\[0\\]"
                                  (alist-get 'text (car (alist-get 'records result))))))))))

(ert-deftest bob/session-cli-send-resumes-the-recorded-profile-without-prompting ()
  (bob/session-cli-test-with-server
    (let* ((mentat-pi-profiles '(("Work" . "/pi/work")
                                ("basic" :directory "/pi/basic" :agent-features nil)))
           (file (make-temp-file "session-cli-resume-"))
           (entry (mentat--registry-entry-create
                   :session-id "closed" :session-file file :root default-directory
                   :pi-directory "/pi/basic"))
           (session (mentat--session-make :ready t))
           (buffer (generate-new-buffer " *session-cli-resumed*"))
           (view (mentat--buffer-make :buffer buffer :session session :status "idle"))
           start-options sent-prompt)
      (unwind-protect
          (cl-letf (((symbol-function 'mentat--registry-list) (lambda (&rest _) (list entry)))
                    ((symbol-function 'mentat--open-live-view) #'ignore)
                    ((symbol-function 'mentat--profile-read-directory)
                     (lambda () (ert-fail "Resume must not prompt for a profile")))
                    ((symbol-function 'mentat--buffer-start)
                     (lambda (_root &rest options)
                       (should-not (mentat--profile-option "/pi/basic" :agent-features t))
                       (setq start-options options)
                       (run-at-time 0.01 nil (plist-get options :ready-handler) view nil)
                       view))
                    ((symbol-function 'mentat--buffer-reconstruct)
                     (lambda (_view _state callback) (funcall callback)))
                    ((symbol-function 'mentat--buffer-submit)
                     (lambda (_view prompt _images accepted _rejected)
                       (setq sent-prompt prompt)
                       (run-at-time 0.01 nil accepted session '((id . "accepted-request")))
                       "accepted-request")))
            (pcase-let* ((`(,exit ,output) (bob/session-cli-test-run "send" "closed" "hello"))
                         (result (json-parse-string output :object-type 'alist)))
              (should (= exit 0))
              (should (equal (alist-get 'request-id result) "accepted-request"))
              (should (eq t (alist-get 'resumed result)))
              (should (equal sent-prompt "hello"))
              (should (equal (plist-get start-options :pi-directory) "/pi/basic"))
              (should (= (length mentat-pi-profiles) 2))))
        (delete-file file)
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest bob/session-cli-errors-and-timeouts-do-not-retry-operations ()
  (bob/session-cli-test-with-server
    (let ((mentat-pi-profiles `(("basic" :directory ,default-directory)))
          starts cancelled)
      (cl-letf (((symbol-function 'mentat-session-manager--starter)
                 (lambda (&rest _)
                   (lambda (_resolve _reject on-cancel)
                     (setq starts (1+ (or starts 0)))
                     (funcall on-cancel (lambda () (setq cancelled t)))))))
        (pcase-let ((`(,exit ,output)
                     (bob/session-cli-test-run "--timeout" "1" "create" default-directory)))
          (should (= exit 1))
          (should (string-match-p "timed out" output))
          (should (= starts 1))
          (should cancelled)
          (should (= (hash-table-count mentat--async-evaluations) 0))))
      (pcase-let ((`(,exit ,output)
                   (bob/session-cli-test-run "create" default-directory "--profile" "missing")))
        (should (= exit 1))
        (should (string-match-p "Unknown Mentat profile" output))
        (should (= starts 1))))))

(ert-deftest bob/session-cli-reload-refuses-a-displayed-session ()
  (skip-unless noninteractive)
  (let* ((entry (mentat--registry-entry-create :session-id "displayed"))
         (view (mentat--buffer-make :buffer (current-buffer) :status "idle")))
    (cl-letf (((symbol-function 'mentat--registry-list) (lambda (&rest _) (list entry)))
              ((symbol-function 'mentat--open-live-view) (lambda (_) view))
              ((symbol-function 'get-buffer-window) (lambda (&rest _) t))
              ((symbol-function 'mentat-session-reload)
               (lambda (&rest _) (ert-fail "Must not reload a displayed session"))))
      (should-error
       (bob/mentat-session-cli-starter
        "{\"command\":\"reload\",\"arguments\":[\"displayed\"],\"keywords\":{}}")
       :type 'user-error))))

(provide 'session-cli-tests)
;;; session-cli-tests.el ends here
