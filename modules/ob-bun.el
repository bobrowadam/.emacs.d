;;; ob-bun.el --- Org-Babel support for Bun (JS/TS runtime) -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2026 Bob

;; Author: Bob
;; Keywords: babel, bun, javascript, typescript
;; Version: 0.2.0

;;; Commentary:
;;
;; Features:
;; - Execute `bun` and `ts` source blocks
;; - Shared `:session` via Bun REPL
;; - `:results output` and `:results value`
;; - `:dir`, `:env`, `:cmd`, `:flags`
;; - `:check yes` TypeScript diagnostics with `bunx --bun tsc --noEmit`
;; - `:imports` prelude injection
;; - `:async yes` background execution (non-session)
;; - Better error messages with source placeholder mapping
;;
;; Node modules:
;; - Prefer `:dir` at your project root so Bun resolves local dependencies.
;; - Optional `:node-modules` sets NODE_PATH explicitly.
;;
;;; Code:

(require 'cl-lib)
(require 'ob)
(require 'ob-eval)
(require 'ob-comint)
(require 'ob-typescript)
(require 'json)
(require 'subr-x)

(defgroup org-babel-bun nil
  "Org Babel Bun integration."
  :group 'org-babel)

(defcustom org-babel-bun-command "bun"
  "Default Bun command."
  :group 'org-babel-bun
  :type 'string)

(defcustom org-babel-bun-repl-args '("repl")
  "Arguments used to start Bun REPL sessions."
  :group 'org-babel-bun
  :type '(repeat string))

(defcustom org-babel-bun-session-prompt-regexp "^[^>\n]*> "
  "Prompt regexp used for Bun REPL comint sessions."
  :group 'org-babel-bun
  :type 'regexp)

(defcustom org-babel-bun-value-indicator "__ORG_BABEL_BUN_VALUE__"
  "Marker used to find value results in Bun output."
  :group 'org-babel-bun
  :type 'string)

(defcustom org-babel-bun-value-format-default "json"
  "Default format for `:results value` in ob-bun.
Supported values:
- \"json\" (default): keep JavaScript/JSON style output
- \"org\": convert to native Org/Elisp structures."
  :group 'org-babel-bun
  :type '(choice (const "json") (const "org")))

(defvar org-babel-bun-async-jobs (make-hash-table :test #'equal)
  "Async Bun jobs keyed by job id.")

(defvar org-babel-default-header-args:bun
  '((:results . "output")
    (:session . "none")
    (:cmd . "bun")
    (:flags . "run")
    (:value-format . "json"))
  "Default header arguments for Bun code blocks.")

(defvar org-babel-default-header-args:ts
  '((:results . "output")
    (:session . "none")
    (:cmd . "bun")
    (:flags . "run")
    (:value-format . "json"))
  "Default header arguments for TypeScript code blocks.")

(add-to-list 'org-babel-tangle-lang-exts '("bun" . "ts"))
(add-to-list 'org-babel-tangle-lang-exts '("ts" . "ts"))

(defun org-babel-variable-assignments:bun (params)
  "Return list of Bun variable assignment statements from PARAMS."
  (org-babel-variable-assignments:typescript params))

(defun org-babel-variable-assignments:ts (params)
  "Return list of TypeScript variable assignment statements from PARAMS."
  (org-babel-variable-assignments:typescript params))

(defun org-babel-bun--plist-like-pairs (value)
  "Return VALUE normalized to list of (KEY . VAL) env pairs."
  (cond
   ((null value) nil)
   ((and (listp value) (consp (car value)))
    (mapcar (lambda (pair)
              (cons (format "%s" (car pair)) (format "%s" (cdr pair))))
            value))
   ((stringp value)
    (cl-loop for tok in (split-string value "[ \t]+" t)
             when (string-match "\\`\\([^=]+\\)=\\(.*\\)\\'" tok)
             collect (cons (match-string 1 tok) (match-string 2 tok))))
   (t nil)))

(defun org-babel-bun--result-type (params)
  "Return normalized result type for PARAMS."
  (or (cdr (assq :result-type params)) "output"))

(defun org-babel-bun--value-result-p (params)
  "Whether PARAMS request `:results value'."
  (string= (org-babel-bun--result-type params) "value"))

(defun org-babel-bun--value-format (params)
  "Return normalized value format from PARAMS."
  (let ((fmt (or (cdr (assq :value-format params))
                 org-babel-bun-value-format-default
                 "json")))
    (downcase (format "%s" fmt))))

(defun org-babel-bun--session-name (session)
  "Normalize SESSION to a buffer name."
  (if (string-prefix-p "*" session) session (format "*%s*" session)))

(defun org-babel-bun--execution-dir (params)
  "Return execution directory from PARAMS."
  (let ((dir (cdr (assq :dir params))))
    (if (and dir (stringp dir) (not (string-empty-p dir)))
        (file-name-as-directory (expand-file-name dir))
      default-directory)))

(defun org-babel-bun--node-modules-dir (params exec-dir)
  "Resolve node_modules path from PARAMS and EXEC-DIR."
  (let* ((nm (cdr (assq :node-modules params)))
         (candidate (if (and (stringp nm) (not (string-empty-p nm)))
                        (expand-file-name nm exec-dir)
                      (expand-file-name "node_modules" exec-dir))))
    (when (file-directory-p candidate) candidate)))

(defun org-babel-bun--process-environment (params exec-dir)
  "Build process environment from PARAMS and EXEC-DIR."
  (let* ((env-pairs (org-babel-bun--plist-like-pairs (cdr (assq :env params))))
         (node-modules-dir (org-babel-bun--node-modules-dir params exec-dir))
         (env (copy-sequence process-environment)))
    (when node-modules-dir
      (setenv "NODE_PATH" node-modules-dir)
      (setq env (cons (format "NODE_PATH=%s" node-modules-dir)
                      (cl-remove-if (lambda (s) (string-prefix-p "NODE_PATH=" s)) env))))
    (dolist (pair env-pairs env)
      (let* ((key (car pair))
             (val (cdr pair))
             (entry (format "%s=%s" key val)))
        (setq env (cons entry (cl-remove-if (lambda (s) (string-prefix-p (concat key "=") s)) env)))))
    env))

(defun org-babel-bun--imports-string (params)
  "Return import prelude text from PARAMS."
  (let ((imports (cdr (assq :imports params))))
    (cond
     ((null imports) "")
     ((stringp imports)
      (concat imports (if (string-suffix-p "\n" imports) "" "\n")))
     ((listp imports)
      (concat (mapconcat (lambda (x) (format "%s" x)) imports "\n") "\n"))
     (t ""))))

(defun org-babel-bun--expand-body (body params)
  "Expand BODY for PARAMS with `:var` and optional `:imports`."
  (concat
   (org-babel-bun--imports-string params)
   (org-babel-expand-body:generic
    body params
    (org-babel-variable-assignments:typescript params))))

(defun org-babel-bun--value-wrapper (body)
  "Wrap BODY to emit a machine-parsable value marker."
  (format
   (concat
    "const __ob_bun_val = (() => {\n%s\n})();\n"
    "if (typeof __ob_bun_val === 'undefined') {\n"
    "  console.log('%sundefined');\n"
    "} else {\n"
    "  try {\n"
    "    console.log('%s' + JSON.stringify(__ob_bun_val));\n"
    "  } catch (_e) {\n"
    "    console.log('%s' + String(__ob_bun_val));\n"
    "  }\n"
    "}")
   body
   org-babel-bun-value-indicator
   org-babel-bun-value-indicator
   org-babel-bun-value-indicator))

(defun org-babel-bun--body-for-result-type (body params)
  "Return BODY wrapped according to `:results` in PARAMS."
  (if (org-babel-bun--value-result-p params)
      (org-babel-bun--value-wrapper body)
    body))

(defun org-babel-bun-read (result)
  "Parse RESULT into an Elisp value when possible."
  (let ((text (string-trim (format "%s" (or result "")))))
    (cond
     ((or (string-prefix-p "{" text) (string-prefix-p "[" text))
      (condition-case nil
          (json-parse-string text :object-type 'alist :array-type 'list :null-object nil :false-object :json-false)
        (error text)))
     (t
      (condition-case nil
          (org-babel-read text)
        (error text))))))

(defun org-babel-bun--extract-value-result (raw)
  "Extract value payload from RAW output."
  (let* ((lines (split-string (or raw "") "\n"))
         (payload (cl-loop for line in lines
                           when (string-prefix-p org-babel-bun-value-indicator line)
                           collect (substring line (length org-babel-bun-value-indicator))
                           into vals
                           finally return (car (last vals)))))
    (or payload "")))

(defun org-babel-bun--strip-ansi (text)
  "Strip ANSI escape sequences from TEXT."
  (let* ((plain (substring-no-properties (or text "")))
         (no-ansi (replace-regexp-in-string "\x1b\\[[0-9;]*[[:alpha:]]" "" plain)))
    (replace-regexp-in-string "\r" "" no-ansi)))

(defun org-babel-bun--normalize-errors (text script-file)
  "Normalize Bun error TEXT replacing SCRIPT-FILE with src block marker."
  (if (and text script-file)
      (replace-regexp-in-string
       (regexp-quote script-file)
       "<org-src-block>"
       text t t)
    text))

(defun org-babel-bun--check-typescript (script-file params exec-dir env)
  "Run optional TypeScript checks for SCRIPT-FILE using PARAMS.
EXEC-DIR and ENV are used for process context."
  (let ((check (cdr (assq :check params))))
    (when (and check (not (equal check "no")) (not (equal check "nil")))
      (let* ((check-cmd (or (cdr (assq :check-cmd params))
                            (format "bunx --bun tsc --noEmit --pretty false %s"
                                    (shell-quote-argument script-file))))
             (default-directory exec-dir)
             (process-environment env)
             (out (org-babel-eval check-cmd "")))
        (unless (string-empty-p (string-trim out))
          (error "TypeScript check failed:\n%s"
                 (org-babel-bun--normalize-errors out script-file)))))))

(defun org-babel-bun--command (script-file params)
  "Build command for SCRIPT-FILE from PARAMS."
  (let* ((cmd (or (cdr (assq :cmd params)) org-babel-bun-command))
         (flags (or (cdr (assq :flags params)) "run"))
         (parts (delq nil
                      (list (and (stringp cmd) (string-trim cmd))
                            (and (stringp flags) (not (string-empty-p (string-trim flags)))
                                 (string-trim flags))
                            (shell-quote-argument script-file)))))
    (mapconcat #'identity parts " ")))

(defun org-babel-bun--make-temp-script (ext exec-dir contents)
  "Write CONTENTS to a temp file with EXT in EXEC-DIR and return its path."
  (let ((script-file (make-temp-file (expand-file-name ".ob-bun-" exec-dir) nil ext)))
    (with-temp-file script-file
      (insert contents))
    script-file))

(defun org-babel-bun--external-exec (body params ext)
  "Execute BODY using Bun process with PARAMS and EXT."
  (let* ((exec-dir (org-babel-bun--execution-dir params))
         (env (org-babel-bun--process-environment params exec-dir))
         (expanded-body (org-babel-bun--expand-body body params))
         (script-body (org-babel-bun--body-for-result-type expanded-body params))
         (script-file (org-babel-bun--make-temp-script ext exec-dir script-body)))
    (unwind-protect
        (let* ((default-directory exec-dir)
               (process-environment env)
               (_ (org-babel-bun--check-typescript script-file params exec-dir env))
               (raw (org-babel-eval (org-babel-bun--command script-file params) "")))
          (org-babel-bun--normalize-errors raw script-file))
      (when (file-exists-p script-file)
        (delete-file script-file)))))

(defun org-babel-bun--make-async-job-id (body params)
  "Generate a stable-ish async job id for BODY and PARAMS."
  (format "ob-bun-%s"
          (substring (secure-hash 'sha1
                                  (format "%s|%s|%s" (float-time) body params))
                     0 12)))

(defun org-babel-bun-async-status (job-id)
  "Return async status plist for JOB-ID."
  (gethash job-id org-babel-bun-async-jobs))

(defun org-babel-bun-async-result (job-id)
  "Return async output string for JOB-ID if available."
  (plist-get (gethash job-id org-babel-bun-async-jobs) :result))

(defun org-babel-bun-async-job-ids ()
  "Return all known async Bun job ids."
  (let (ids)
    (maphash (lambda (k _v) (push k ids)) org-babel-bun-async-jobs)
    (nreverse ids)))

(defun org-babel-bun-async-job-id (message)
  "Extract async job id from MESSAGE returned by ob-bun."
  (when (and (stringp message)
             (string-match "started: \\([^ ]+\\)" message))
    (match-string 1 message)))

(defun org-babel-bun-async-clear-finished ()
  "Remove finished async jobs from `org-babel-bun-async-jobs'."
  (interactive)
  (let ((removed 0))
    (maphash
     (lambda (k v)
       (when (memq (plist-get v :status) '(done failed))
         (remhash k org-babel-bun-async-jobs)
         (setq removed (1+ removed))))
     org-babel-bun-async-jobs)
    (when (called-interactively-p 'interactive)
      (message "ob-bun: removed %d finished async job(s)" removed))
    removed))

(defun org-babel-bun-async-show-jobs ()
  "Open a buffer with async ob-bun job status and results preview."
  (interactive)
  (let ((buf (get-buffer-create "*ob-bun-async-jobs*")))
    (with-current-buffer buf
      (read-only-mode -1)
      (erase-buffer)
      (insert (format "%-18s  %-8s  %s\n" "Job ID" "Status" "Preview"))
      (insert (make-string 80 ?-))
      (insert "\n")
      (dolist (id (org-babel-bun-async-job-ids))
        (let* ((job (org-babel-bun-async-status id))
               (status (or (plist-get job :status) 'unknown))
               (result (or (plist-get job :result) ""))
               (preview (replace-regexp-in-string
                         "[\n\r]+" " "
                         (truncate-string-to-width result 50 nil nil t))))
          (insert (format "%-18s  %-8s  %s\n" id status preview))))
      (goto-char (point-min))
      (special-mode))
    (pop-to-buffer buf)))

(defun org-babel-bun--start-async-job (body params ext)
  "Start non-session async Bun execution for BODY using PARAMS and EXT."
  (let* ((exec-dir (org-babel-bun--execution-dir params))
         (env (org-babel-bun--process-environment params exec-dir))
         (expanded-body (org-babel-bun--expand-body body params))
         (script-body (org-babel-bun--body-for-result-type expanded-body params))
         (script-file (org-babel-bun--make-temp-script ext exec-dir script-body))
         (job-id (org-babel-bun--make-async-job-id body params))
         (buffer (get-buffer-create (format "*ob-bun-async-%s*" job-id)))
         (cmd (org-babel-bun--command script-file params)))
    (puthash job-id (list :status 'running :buffer buffer :script script-file :result nil)
             org-babel-bun-async-jobs)
    (let ((default-directory exec-dir)
          (process-environment env))
      (make-process
       :name (format "ob-bun-%s" job-id)
       :buffer buffer
       :command (list shell-file-name shell-command-switch cmd)
       :noquery t
       :sentinel
       (lambda (proc event)
         (when (memq (process-status proc) '(exit signal))
           (let* ((buf (process-buffer proc))
                  (raw (if (buffer-live-p buf)
                           (with-current-buffer buf (buffer-string))
                         ""))
                  (final (org-babel-bun--normalize-errors raw script-file))
                  (status (if (zerop (process-exit-status proc)) 'done 'failed)))
            (puthash job-id
                     (list :status status
                            :event event
                            :buffer buf
                            :script script-file
                            :result (org-babel-bun--strip-ansi final))
                      org-babel-bun-async-jobs)
             (when (file-exists-p script-file)
               (delete-file script-file)))))
       :coding 'utf-8
       :connection-type 'pipe))
    (process-put (get-process (format "ob-bun-%s" job-id)) 'default-directory exec-dir)
    (process-put (get-process (format "ob-bun-%s" job-id)) 'env env)
    (format "Async Bun job started: %s (check with (org-babel-bun-async-status %S))"
            job-id job-id)))

(defun org-babel-bun--session-eoe ()
  "Return per-call session end marker."
  (format "__ORG_BABEL_BUN_EOE__%s" (substring (secure-hash 'sha1 (format "%s" (float-time))) 0 8)))

(defun org-babel-bun-initiate-session (&optional session params)
  "Create or reuse Bun SESSION according to PARAMS."
  (if (not (and session (not (string= session "none"))))
      nil
    (let* ((buffer-name (org-babel-bun--session-name session))
           (buffer (get-buffer-create buffer-name)))
      (unless (org-babel-comint-buffer-livep buffer)
        (let* ((cmd (or (cdr (assq :cmd params)) org-babel-bun-command))
               (repl-args (or (cdr (assq :repl-args params)) org-babel-bun-repl-args))
               (args (cond
                      ((listp repl-args) repl-args)
                      ((stringp repl-args) (split-string repl-args "[ \t]+" t))
                      (t org-babel-bun-repl-args)))
               (exec-dir (org-babel-bun--execution-dir params))
               (env (org-babel-bun--process-environment params exec-dir)))
          (let ((default-directory exec-dir)
                (process-environment env))
            (apply #'make-comint-in-buffer "ob-bun" buffer cmd nil args))
          (with-current-buffer buffer
            (setq-local comint-prompt-regexp org-babel-bun-session-prompt-regexp)
            (setq-local org-babel-comint-prompt-regexp-fallback org-babel-bun-session-prompt-regexp))))
      buffer-name)))

(defun org-babel-prep-session:bun (session params)
  "Prepare Bun SESSION by assigning vars from PARAMS."
  (let* ((session (org-babel-bun-initiate-session session params))
         (var-lines (org-babel-variable-assignments:bun params)))
    (when session
      (org-babel-comint-in-buffer session
        (goto-char (point-max))
        (dolist (var var-lines)
          (insert var)
          (comint-send-input nil t)
          (org-babel-comint-wait-for-output session))))
    session))

(defun org-babel-prep-session:ts (session params)
  "Prepare TypeScript SESSION by assigning vars from PARAMS."
  (org-babel-prep-session:bun session params))

(defun org-babel-bun--clean-session-output (raw eoe)
  "Clean RAW session output and strip EOE marker."
  (let* ((text (org-babel-bun--strip-ansi raw))
         ;; Remove echoed EOE print command first.
         (text (replace-regexp-in-string
                (format ".*console\\.log(\\\"%s\\\").*\n?" (regexp-quote eoe))
                ""
                text))
         ;; Ignore dangling repl output that may appear after EOE.
         (text (car (split-string text (regexp-quote eoe))))
         (lines (split-string text "\n" t))
         (normalized
          (mapcar
           (lambda (line)
             (let ((l (string-trim-left line)))
               (if (string-prefix-p "> " l)
                   (string-trim (substring l 2))
                 (string-trim l))))
           lines))
         (filtered
          (cl-remove-if
           (lambda (line)
             (or (string-empty-p line)
                 (string= line "undefined")
                 (string-match-p "__ORG_BABEL_BUN_EOE__" line)
                 (string-prefix-p "console.log(\"" line)))
           normalized)))
    (string-join filtered "\n")))

(defun org-babel-bun--session-eval (session code)
  "Evaluate CODE in Bun SESSION and return raw output."
  (let ((eoe (org-babel-bun--session-eoe)))
    (org-babel-comint-in-buffer session
      (goto-char (process-mark (get-buffer-process (current-buffer))))
      (delete-region (point) (point-max)))
    (org-babel-bun--clean-session-output
     (let ((result (org-babel-comint-with-output
                       (session eoe t code)
                     (insert (org-babel-chomp code))
                     (comint-send-input nil t)
                     (insert (format "console.log(%S)" eoe))
                     (comint-send-input nil t))))
       (if (listp result) (string-join result "\n") result))
     eoe)))

(defun org-babel-bun--execute-in-session (body params)
  "Execute BODY inside Bun session using PARAMS."
  (let* ((session (org-babel-prep-session:bun (cdr (assq :session params)) params))
         (expanded-body (org-babel-bun--expand-body body params))
         (code (org-babel-bun--body-for-result-type expanded-body params)))
    (org-babel-bun--session-eval session code)))

(defun org-babel-bun--finalize-result (raw params)
  "Convert RAW command result using PARAMS."
  (if (org-babel-bun--value-result-p params)
      (let* ((payload (org-babel-bun--extract-value-result raw))
             (fmt (org-babel-bun--value-format params)))
        (if (string= fmt "org")
            (org-babel-bun-read payload)
          payload))
    (org-babel-bun--strip-ansi raw)))

(defun org-babel-bun--execute (body params ext)
  "Core executor for BODY with PARAMS and source EXT."
  (let* ((session (cdr (assq :session params)))
         (async (cdr (assq :async params))))
    (cond
     ((and async (not (string= (or session "none") "none")))
      (error ":async with :session is not supported for ob-bun"))
     (async
      (org-babel-bun--start-async-job body params ext))
     (t
      (let ((raw (if (and session (not (string= session "none")))
                     (org-babel-bun--execute-in-session body params)
                   (org-babel-bun--external-exec body params ext))))
        (org-babel-bun--finalize-result raw params))))))

(defun org-babel-execute:bun (body params)
  "Execute Bun BODY according to PARAMS."
  (org-babel-bun--execute body params ".ts"))

(defun org-babel-execute:ts (body params)
  "Execute TypeScript BODY according to PARAMS."
  (org-babel-bun--execute body params ".ts"))

(defun org-babel-ts-initiate-session (&optional session params)
  "Start or reuse TypeScript SESSION with PARAMS."
  (org-babel-bun-initiate-session session params))

(provide 'ob-bun)
;;; ob-bun.el ends here
