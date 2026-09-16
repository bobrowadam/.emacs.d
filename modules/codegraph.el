;;; codegraph.el --- Stateful local code queries -*- lexical-binding: t; -*-

;;; Commentary:
;; Private SDK host.  This module does not depend on Mentat.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)

(defconst bob/codegraph-host-file
  (expand-file-name "codegraph-host.cjs"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Path to the CodeGraph SDK host.")

(defconst bob/codegraph-start-timeout 300
  "Seconds allowed for CodeGraph startup and initial indexing.")

(defconst bob/codegraph-stop-timeout 3
  "Seconds allowed for graceful CodeGraph shutdown.")

(defconst bob/codegraph-operations '("start" "status" "search" "context")
  "Operations supported by `bob/codegraph-request'.")

(defvar bob/codegraph-processes (make-hash-table :test #'equal)
  "CodeGraph processes keyed by canonical worktree directory.")

(defvar bob/codegraph-request-id 0
  "Last request identifier sent to a CodeGraph host.")

(defun bob/codegraph-root (directory)
  "Return the canonical local DIRECTORY without selecting a buffer."
  (when (or (file-remote-p directory) (not (file-directory-p directory)))
    (user-error "CodeGraph needs an existing local directory"))
  (file-name-as-directory (file-truename directory)))

(defun bob/codegraph-remove-waiter (process property token)
  "Remove TOKEN from PROCESS callback list PROPERTY."
  (process-put process property
               (delq token (process-get process property))))

(defun bob/codegraph-resolve-waiters (process property value)
  "Resolve PROCESS callback list PROPERTY with VALUE."
  (let ((waiters (process-get process property)))
    (process-put process property nil)
    (dolist (waiter waiters)
      (funcall (nth 1 waiter) value))))

(defun bob/codegraph-reject-waiters (process property reason)
  "Reject PROCESS callback list PROPERTY with REASON."
  (let ((waiters (process-get process property)))
    (process-put process property nil)
    (dolist (waiter waiters)
      (funcall (nth 2 waiter) reason))))

(defun bob/codegraph-reject-pending (process reason)
  "Reject all pending query callbacks on PROCESS with REASON."
  (let ((pending (process-get process 'pending)) callbacks)
    (when pending
      (maphash (lambda (_id pair) (push (cdr pair) callbacks)) pending)
      (clrhash pending)
      (dolist (callback callbacks) (funcall callback reason)))))

(defun bob/codegraph-cancel-timer (process property)
  "Cancel the timer stored on PROCESS under PROPERTY."
  (when-let* ((timer (process-get process property)))
    (cancel-timer timer)
    (process-put process property nil)))

(defun bob/codegraph-handle-ready (process reply)
  "Mark PROCESS ready from host REPLY."
  (when (eq (process-get process 'state) 'starting)
    (bob/codegraph-cancel-timer process 'start-timer)
    (process-put process 'state 'ready)
    (bob/codegraph-resolve-waiters process 'start-waiters reply)))

(defun bob/codegraph-filter (process chunk)
  "Decode complete JSONL replies from PROCESS in CHUNK."
  (condition-case err
      (let ((text (concat (process-get process 'tail) chunk)))
        (when (> (string-bytes text) (* 4 1024 1024))
          (error "CodeGraph reply exceeded 4 MiB"))
        (while (string-match (string 10) text)
          (let* ((end (match-beginning 0))
                 (reply (json-parse-string
                         (substring text 0 end) :object-type 'alist
                         :array-type 'list :null-object nil
                         :false-object :false)))
            (setq text (substring text (1+ end)))
            (if (equal (alist-get 'event reply) "ready")
                (bob/codegraph-handle-ready process reply)
              (let* ((id (alist-get 'id reply))
                     (pending (process-get process 'pending))
                     (pair (and pending (gethash id pending))))
                (when pair
                  (remhash id pending)
                  (if-let* ((reason (alist-get 'error reply)))
                      (funcall (cdr pair) reason)
                    (funcall (car pair) reply)))))))
        (process-put process 'tail text))
    (error
     (bob/codegraph-reject-pending process (error-message-string err))
     (bob/codegraph-reject-waiters process 'start-waiters
                                   (error-message-string err))
     (when (process-live-p process) (delete-process process)))))

(defun bob/codegraph-process-exited (process event errors)
  "Finish PROCESS after observed exit EVENT and clean up ERRORS."
  (bob/codegraph-cancel-timer process 'start-timer)
  (bob/codegraph-cancel-timer process 'stop-timer)
  (let* ((stopping (eq (process-get process 'state) 'stopping))
         (details (when (buffer-live-p errors)
                    (with-current-buffer errors
                      (string-trim
                       (buffer-substring-no-properties
                        (max (point-min) (- (point-max) 4000))
                        (point-max))))))
         (reason (string-trim
                  (concat "CodeGraph host " event
                          (if (string-empty-p (or details ""))
                              "" (concat ": " details))))))
    (process-put process 'state 'stopped)
    (bob/codegraph-reject-pending process reason)
    (bob/codegraph-reject-waiters process 'start-waiters reason)
    (let ((root (process-get process 'root)))
      (when (eq process (gethash root bob/codegraph-processes))
        (remhash root bob/codegraph-processes)))
    (if stopping
        (bob/codegraph-resolve-waiters process 'stop-waiters
                                       '((stopped . t)))
      (bob/codegraph-reject-waiters process 'stop-waiters reason))
    (when (buffer-live-p errors) (kill-buffer errors))))

(defun bob/codegraph-start-expired (process)
  "Terminate PROCESS after its startup deadline."
  (when (and (process-live-p process)
             (eq (process-get process 'state) 'starting))
    (process-put process 'state 'stopping)
    (bob/codegraph-reject-waiters process 'start-waiters
                                  "CodeGraph startup timed out")
    (signal-process process 'SIGTERM)
    (process-put process 'stop-timer
                 (run-at-time bob/codegraph-stop-timeout nil
                              #'bob/codegraph-stop-expired process))))

(defun bob/codegraph-stop-expired (process)
  "Force termination when PROCESS did not stop before its deadline."
  (when (process-live-p process)
    (delete-process process)))

(defun bob/codegraph-spawn (root)
  "Spawn a CodeGraph host for canonical ROOT in the starting state."
  (let* ((cli (or (executable-find "codegraph")
                  (user-error "CodeGraph is not installed")))
         (sdk (file-name-directory (file-truename cli)))
         (node (or (executable-find "node")
                   (user-error "Node is not installed")))
         (default-directory root)
         (process-environment (cons "DO_NOT_TRACK=1" process-environment))
         (errors (generate-new-buffer " *codegraph-log*"))
         process)
    (setq process
          (make-process
           :name "codegraph" :buffer nil :stderr errors
           :command (list node "--liftoff-only" bob/codegraph-host-file sdk root)
           :coding 'utf-8-unix :connection-type 'pipe :noquery t
           :filter #'bob/codegraph-filter
           :sentinel (lambda (proc event)
                       (unless (process-live-p proc)
                         (bob/codegraph-process-exited proc event errors)))))
    (process-put process 'root root)
    (process-put process 'errors errors)
    (process-put process 'state 'starting)
    (process-put process 'tail "")
    (process-put process 'pending (make-hash-table :test #'eql))
    (process-put process 'start-waiters nil)
    (process-put process 'stop-waiters nil)
    (process-put process 'start-timer
                 (run-at-time bob/codegraph-start-timeout nil
                              #'bob/codegraph-start-expired process))
    (puthash root process bob/codegraph-processes)
    process))

(defun bob/codegraph-add-waiter (process property resolve reject on-cancel)
  "Add callbacks for PROCESS under PROPERTY and register ON-CANCEL."
  (let ((token (list (cl-incf bob/codegraph-request-id) resolve reject)))
    (process-put process property
                 (cons token (process-get process property)))
    (funcall on-cancel
             (lambda ()
               (bob/codegraph-remove-waiter process property token)))))

(defun bob/codegraph-start-request (root resolve reject on-cancel)
  "Start ROOT or observe its current host through callback functions."
  (let ((process (gethash root bob/codegraph-processes)))
    (cond
     ((null process)
      (setq process (bob/codegraph-spawn root))
      (bob/codegraph-add-waiter process 'start-waiters
                                resolve reject on-cancel))
     ((eq (process-get process 'state) 'starting)
      (bob/codegraph-add-waiter process 'start-waiters
                                resolve reject on-cancel))
     ((eq (process-get process 'state) 'ready)
      (bob/codegraph-send-query process "status" nil
                                resolve reject on-cancel))
     (t (funcall reject "CodeGraph host is stopping")))))

(defun bob/codegraph-send-query (process operation query resolve reject on-cancel)
  "Send OPERATION and QUERY to ready PROCESS using callback functions."
  (if (not (and (process-live-p process)
                (eq (process-get process 'state) 'ready)))
      (funcall reject "CodeGraph host is not ready")
    (let* ((id (cl-incf bob/codegraph-request-id))
           (pending (process-get process 'pending)))
      (puthash id (cons resolve reject) pending)
      (funcall on-cancel (lambda () (remhash id pending)))
      (condition-case err
          (process-send-string
           process
           (concat (json-serialize
                    `((id . ,id) (operation . ,operation) (query . ,query)))
                   (string 10)))
        (error
         (remhash id pending)
         (funcall reject (error-message-string err)))))))

(defun bob/codegraph-stop-request (root resolve reject on-cancel)
  "Stop ROOT and resolve only after its process exits."
  (let ((process (gethash root bob/codegraph-processes)))
    (if (null process)
        (funcall resolve '((stopped . t)))
      (bob/codegraph-add-waiter process 'stop-waiters
                                resolve reject on-cancel)
      (unless (eq (process-get process 'state) 'stopping)
        (process-put process 'state 'stopping)
        (bob/codegraph-cancel-timer process 'start-timer)
        (bob/codegraph-reject-waiters process 'start-waiters
                                      "CodeGraph startup was stopped")
        (bob/codegraph-reject-pending process "CodeGraph host is stopping")
        (if (process-live-p process)
            (progn
              (signal-process process 'SIGTERM)
              (process-put process 'stop-timer
                           (run-at-time bob/codegraph-stop-timeout nil
                                        #'bob/codegraph-stop-expired process)))
          (bob/codegraph-process-exited process "exited"
                                        (process-get process 'errors)))))))

(defun bob/codegraph-request (directory operation query)
  "Return a callback starter for OPERATION and QUERY in DIRECTORY.
Start and stop observe process events.  Other operations require a ready host."
  (unless (member operation bob/codegraph-operations)
    (user-error "Unknown CodeGraph operation: %s
Use one of %s"
                operation bob/codegraph-operations))
  (when (and (member operation '("search" "context"))
             (or (not (stringp query)) (string-blank-p query)))
    (user-error "CodeGraph search and context require a query"))
  (let ((root (bob/codegraph-root directory)))
    (lambda (resolve reject on-cancel)
      (if (equal operation "start")
          (bob/codegraph-start-request root resolve reject on-cancel)
        (let ((process (gethash root bob/codegraph-processes)))
          (if process
              (bob/codegraph-send-query process operation query
                                        resolve reject on-cancel)
            (funcall reject "Start CodeGraph for this directory first")))))))

(defun bob/codegraph-stop (directory)
  "Return a callback starter that stops DIRECTORY after observed host exit."
  (let ((root (bob/codegraph-root directory)))
    (lambda (resolve reject on-cancel)
      (bob/codegraph-stop-request root resolve reject on-cancel))))

(defun bob/codegraph-stop-all ()
  "Signal all CodeGraph hosts when Emacs exits."
  (maphash (lambda (_root process)
             (when (process-live-p process)
               (signal-process process 'SIGTERM)))
           bob/codegraph-processes))

(add-hook 'kill-emacs-hook #'bob/codegraph-stop-all)

(provide 'codegraph)
;;; codegraph.el ends here
