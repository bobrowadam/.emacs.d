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

(defvar bob/codegraph-processes (make-hash-table :test #'equal)
  "CodeGraph processes keyed by canonical worktree directory.")

(defvar bob/codegraph-request-id 0
  "Last request identifier sent to a CodeGraph host.")

(defun bob/codegraph-root (directory)
  "Return the canonical local DIRECTORY without selecting a buffer."
  (when (or (file-remote-p directory) (not (file-directory-p directory)))
    (user-error "CodeGraph needs an existing local directory"))
  (file-name-as-directory (file-truename directory)))

(defun bob/codegraph-finish (process reason)
  "Reject pending requests on PROCESS with REASON."
  (let ((pending (process-get process 'pending)))
    (when pending
      (let (callbacks)
        (maphash (lambda (_id pair) (push (cdr pair) callbacks)) pending)
        (clrhash pending)
        (dolist (callback callbacks) (funcall callback reason)))))
  (let ((root (process-get process 'root)))
    (when (eq process (gethash root bob/codegraph-processes))
      (remhash root bob/codegraph-processes))))

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
                         :false-object :false))
                 (id (alist-get 'id reply))
                 (pending (process-get process 'pending))
                 (pair (gethash id pending)))
            (setq text (substring text (1+ end)))
            (when pair
              (remhash id pending)
              (if-let* ((reason (alist-get 'error reply)))
                  (funcall (cdr pair) reason)
                (funcall (car pair) reply)))))
        (process-put process 'tail text))
    (error
     (bob/codegraph-finish process (error-message-string err))
     (delete-process process))))

(defun bob/codegraph-start (directory)
  "Start or reuse the graph host for DIRECTORY.
Create a local index when absent.  Startup installs a watcher and syncs once."
  (let* ((root (bob/codegraph-root directory))
         (existing (gethash root bob/codegraph-processes)))
    (if (and existing (process-live-p existing))
        existing
      (let* ((cli (or (executable-find "codegraph")
                      (user-error "CodeGraph is not installed")))
             (sdk (file-name-directory (file-truename cli)))
             (node (or (executable-find "node")
                       (user-error "Node is not installed")))
             (default-directory root)
             (process-environment (cons "DO_NOT_TRACK=1" process-environment))
             (errors (generate-new-buffer " *codegraph-log*"))
             (process
              (make-process
               :name "codegraph" :buffer nil :stderr errors
               :command (list node "--liftoff-only" bob/codegraph-host-file sdk root)
               :coding 'utf-8-unix :connection-type 'pipe :noquery t
               :filter #'bob/codegraph-filter
               :sentinel
               (lambda (proc event)
                 (unless (process-live-p proc)
                   (let ((details (when (buffer-live-p errors)
                                    (with-current-buffer errors
                                      (buffer-substring-no-properties
                                       (max (point-min) (- (point-max) 4000))
                                       (point-max))))))
                     (bob/codegraph-finish proc (concat event details)))
                   (when (buffer-live-p errors) (kill-buffer errors)))))))
        (process-put process 'root root)
        (process-put process 'tail "")
        (process-put process 'pending (make-hash-table :test #'eql))
        (puthash root process bob/codegraph-processes)
        process))))

(defun bob/codegraph-stop (directory)
  "Stop the graph host for DIRECTORY without deleting its index."
  (let ((process (gethash (bob/codegraph-root directory)
                          bob/codegraph-processes)))
    (when process
      (bob/codegraph-finish process "CodeGraph host stopped")
      (when (process-live-p process) (signal-process process 'SIGTERM)))
    '((stopped . t))))

(defconst code-graph-operations '("start" "status" "search" "context"))
(defun bob/codegraph-request (directory operation query)
  "Return a callback starter for OPERATION and QUERY in DIRECTORY.
Only start creates a host.  Cancellation stops that start, or abandons a query."
  (unless (member operation code-graph-operations)
    (user-error "Unknown CodeGraph operation: %s\nUse one of %s" operation code-graph-operations))
  (when (and (member operation '("search" "context"))
             (or (not (stringp query)) (string-blank-p query)))
    (user-error "CodeGraph search and context require a query"))
  (let ((root (bob/codegraph-root directory)))
    (lambda (resolve reject on-cancel)
      (let* ((existing (gethash root bob/codegraph-processes))
             (new (and (equal operation "start")
                       (not (and existing (process-live-p existing)))))
             (process (if new (bob/codegraph-start root) existing))
             (id (cl-incf bob/codegraph-request-id)))
        (unless (and process (process-live-p process))
          (user-error "Start CodeGraph for this directory first"))
        (puthash id (cons resolve reject) (process-get process 'pending))
        (funcall on-cancel
                 (lambda ()
                   (remhash id (process-get process 'pending))
                   (when new (bob/codegraph-stop root))))
        (process-send-string
         process
         (concat (json-serialize
                  `((id . ,id)
                    (operation . ,(if (equal operation "start")
                                      "status" operation))
                    (query . ,query)))
                 (string 10)))))))

(defun bob/codegraph-stop-all ()
  "Stop all CodeGraph hosts when Emacs exits."
  (dolist (root (hash-table-keys bob/codegraph-processes))
    (bob/codegraph-stop root)))

(add-hook 'kill-emacs-hook #'bob/codegraph-stop-all)

(provide 'codegraph)
;;; codegraph.el ends here
