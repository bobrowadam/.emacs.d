;;; codegraph.el --- Mentat CodeGraph experiment -*- lexical-binding: t; -*-

;;; Commentary:
;; Non-MCP queries backed by one SDK host per worktree.

;;; Code:

(require 'mentat-elisp-library)
(require 'codegraph
         (expand-file-name "modules/codegraph.el" user-emacs-directory))

(mentat-defun mentat-codegraph (operation &optional query directory)
  "Query a persistent local CodeGraph with explicit freshness status.
OPERATION is start, status, search, context, or stop.  QUERY is required for
search and context.  DIRECTORY defaults to the originating working directory.
Start creates an ignored local index if absent, watches files, and syncs once.
Queries require a running host and do not force sync.  Inspect freshness in
every result: pending files, indexing, or degraded watching can mean stale
results.  Read affected files directly.  Stop retains the index.  No MCP,
telemetry, agent setup, or automatic session prompts are used."
  (:execution async :display "CodeGraph Query")
  (let ((root (or directory default-directory)))
    (if (equal operation "stop")
        (bob/codegraph-stop root)
      (bob/codegraph-request root operation query))))

(provide 'mentat-codegraph)
;;; codegraph.el ends here
