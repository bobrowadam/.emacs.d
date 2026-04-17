;;; bob-gptel-tools.el --- Pi-style gptel tool suite -*- lexical-binding: t; -*-

;; A small set of gptel tools that mirror the basic tool set of the pi
;; coding agent (bash, edit, find, grep, ls, read, write) plus
;; `elisp_eval' for introspecting the running Emacs.
;;
;; The tool names and argument shapes are chosen to match pi so the
;; LLM can reuse the same mental model and docs across pi and gptel.
;;
;; The `elisp_eval' tool is adapted from karthink/gptel-agent's `Eval'
;; tool (https://github.com/karthink/gptel-agent).
;;
;; Load via `(require 'bob-gptel-tools)' after gptel is available.
;; Registration happens at load time via `gptel-make-tool', which
;; populates `gptel--known-tools'.  Call `bob/gptel-tools-default-set'
;; to install the whole suite into `gptel-tools'.

;;; Code:

(require 'gptel)
(require 'cl-lib)
(require 'subr-x)
(require 'seq)

(defgroup bob-gptel-tools nil
  "Pi-style gptel tool suite."
  :group 'gptel)

(defcustom bob/gptel-tools-output-max-bytes 50000
  "Maximum bytes returned by filesystem/shell tools before truncation.
Mirrors pi's truncation cap."
  :type 'integer
  :group 'bob-gptel-tools)

(defcustom bob/gptel-tools-bash-timeout 120
  "Default timeout (seconds) for the `bash' tool."
  :type 'integer
  :group 'bob-gptel-tools)

(defcustom bob/gptel-tools-ls-default-limit 1000
  "Default entry limit for the `ls' tool."
  :type 'integer
  :group 'bob-gptel-tools)

(defcustom bob/gptel-tools-find-default-limit 1000
  "Default result limit for the `find' tool."
  :type 'integer
  :group 'bob-gptel-tools)

(defcustom bob/gptel-tools-grep-default-limit 1000
  "Default match limit for the `grep' tool."
  :type 'integer
  :group 'bob-gptel-tools)

;;; Helpers --------------------------------------------------------------

(defun bob/gptel-tools--truncate (s)
  "Truncate string S to `bob/gptel-tools-output-max-bytes' with a marker."
  (if (and (stringp s)
           (> (string-bytes s) bob/gptel-tools-output-max-bytes))
      (concat (substring s 0 bob/gptel-tools-output-max-bytes)
              (format "\n[... truncated, output exceeded %d bytes ...]"
                      bob/gptel-tools-output-max-bytes))
    s))

(defun bob/gptel-tools--expand (path)
  "Expand PATH, respecting ~ and relative paths from `default-directory'."
  (expand-file-name (substitute-in-file-name path)))

;;; Async process registry ---------------------------------------------
;;
;; Subprocess-based tools (`bash', `find', `grep', `jina_reader') run
;; asynchronously so the main Emacs thread stays responsive while the
;; LLM is waiting on a tool result.  Every in-flight process is tracked
;; in `bob/gptel-tools--active-processes' so the user can list or kill
;; them interactively via `bob/gptel-abort'.

(defvar bob/gptel-tools--active-processes nil
  "Alist of (PROCESS . PLIST) for in-flight async tool calls.

PLIST contains `:tool', `:label' (short description), `:started'
(float-time) and `:timer' (the timeout timer, if any).")

(defun bob/gptel-tools--register-process (proc plist)
  "Remember PROC with metadata PLIST until it exits."
  (push (cons proc plist) bob/gptel-tools--active-processes))

(defun bob/gptel-tools--unregister-process (proc)
  "Forget PROC and cancel its timeout timer, if any."
  (when-let* ((entry (assq proc bob/gptel-tools--active-processes))
              (timer (plist-get (cdr entry) :timer)))
    (when (timerp timer) (cancel-timer timer)))
  (setq bob/gptel-tools--active-processes
        (assq-delete-all proc bob/gptel-tools--active-processes)))

(cl-defun bob/gptel-tools--run-async-process
    (callback &key tool label program args timeout input)
  "Run PROGRAM with ARGS asynchronously and call CALLBACK with the result.

CALLBACK is invoked with a single string argument -- the combined
stdout+stderr of the process, truncated per
`bob/gptel-tools-output-max-bytes', followed by a short status
suffix.  If the process is killed (by timeout or by the user via
`bob/gptel-abort'), the callback receives an explanatory error
string instead.

TOOL and LABEL are used by `bob/gptel-abort' when listing
running processes.  TIMEOUT, when non-nil, specifies a hard wall
clock in seconds; the process is killed with SIGTERM (then
SIGKILL) when it elapses.  INPUT, when non-nil, is sent to the
process's stdin.

Returns the `process' object."
  (let* ((buf (generate-new-buffer
               (format " *gptel-tool:%s*" (or tool "proc"))))
         (killed-by (list nil))      ;boxed symbol: 'timeout or 'user
         proc timer)
    (setq proc
          (make-process
           :name (format "gptel-tool-%s" (or tool "proc"))
           :buffer buf
           :command (cons program args)
           :connection-type 'pipe
           :noquery t
           :sentinel
           (lambda (p _event)
             (when (memq (process-status p) '(exit signal))
               (bob/gptel-tools--unregister-process p)
               (let* ((status (process-exit-status p))
                      (raw (with-current-buffer (process-buffer p)
                             (buffer-string)))
                      (why (car killed-by))
                      (body
                       (pcase why
                         ('timeout
                          (format "[%s killed after %ds timeout]\n%s"
                                  (or tool "tool") timeout
                                  (string-trim-right raw)))
                         ('user
                          (format "[%s aborted by user]\n%s"
                                  (or tool "tool")
                                  (string-trim-right raw)))
                         (_
                          (format "%s\n[exit: %s]"
                                  (string-trim-right raw) status)))))
                 (kill-buffer (process-buffer p))
                 (funcall callback (bob/gptel-tools--truncate body)))))))
    (when input
      (process-send-string proc input)
      (process-send-eof proc))
    (when (and timeout (> timeout 0))
      (setq timer
            (run-at-time
             timeout nil
             (lambda ()
               (when (process-live-p proc)
                 (setcar killed-by 'timeout)
                 (delete-process proc))))))
    (bob/gptel-tools--register-process
     proc (list :tool tool :label label :timeout timeout
                :started (float-time) :timer timer
                :killed-by killed-by))
    proc))

(defun bob/gptel-tools--proc-entry-label (entry)
  "Return a human-readable label for tool-process ENTRY."
  (let* ((proc (car entry))
         (pl (cdr entry))
         (tool (plist-get pl :tool))
         (label (plist-get pl :label))
         (started (plist-get pl :started))
         (age (truncate (- (float-time) (or started (float-time))))))
    (format "[%s] %s  (pid %s, %ds)"
            (or tool "tool") (or label "")
            (or (process-id proc) "?") age)))

(defun bob/gptel-tools-kill-process (proc &optional reason)
  "Kill tool subprocess PROC.  REASON defaults to 'user."
  (interactive
   (list (let* ((entries bob/gptel-tools--active-processes)
                (_ (unless entries (user-error "No tool processes running")))
                (choices (mapcar
                          (lambda (e)
                            (cons (bob/gptel-tools--proc-entry-label e)
                                  (car e)))
                          entries))
                (pick (completing-read "Kill tool process: " choices nil t)))
           (cdr (assoc pick choices)))))
  (when (and proc (process-live-p proc))
    (when-let* ((entry (assq proc bob/gptel-tools--active-processes))
                (kb (plist-get (cdr entry) :killed-by)))
      (setcar kb (or reason 'user)))
    (delete-process proc)))

(defun bob/gptel-tools-kill-all-processes (&optional reason)
  "Kill every live tool subprocess.  REASON defaults to 'user."
  (interactive)
  (dolist (entry (copy-sequence bob/gptel-tools--active-processes))
    (bob/gptel-tools-kill-process (car entry) reason))
  (when (called-interactively-p 'interactive)
    (message "Killed all tool subprocesses")))

;;;###autoload
(defun bob/gptel-abort (&optional buffer)
  "Cancel an in-flight gptel request and kill any running tool subprocesses.

With BUFFER, operate on that buffer; otherwise use the current
buffer.  If tool subprocesses are alive (e.g. a `grep' scanning a
giant tree), this kills them first so `gptel-abort' returns
immediately instead of blocking.

Bound to `C-c g k' in the user's config."
  (interactive)
  (let ((n (length bob/gptel-tools--active-processes)))
    (when (> n 0)
      (bob/gptel-tools-kill-all-processes 'user)
      (message "Killed %d tool subprocess%s" n (if (= n 1) "" "es"))))
  (when (fboundp 'gptel-abort)
    (ignore-errors (gptel-abort (or buffer (current-buffer))))))

(defun bob/gptel-tools--format-diff (old-content new-content)
  "Return a pi-style diff string between OLD-CONTENT and NEW-CONTENT.

Format mirrors pi's `generateDiffString' (see pi's
`core/tools/edit-diff.js'):
  +NN added line
  -NN removed line
   NN unchanged context line
     ... (between non-adjacent hunks)

Uses the system `diff -U4'.  Returns an empty string if the contents
are identical."
  (if (string= old-content new-content)
      ""
    (let ((old-file (make-temp-file "gptel-diff-old-"))
          (new-file (make-temp-file "gptel-diff-new-")))
      (unwind-protect
          (let (raw)
            (with-temp-file old-file (insert old-content))
            (with-temp-file new-file (insert new-content))
            (with-temp-buffer
              (call-process "diff" nil t nil "-U4"
                            (file-local-name old-file)
                            (file-local-name new-file))
              (setq raw (buffer-string)))
            (bob/gptel-tools--reformat-unified-diff raw new-content))
        (ignore-errors (delete-file old-file))
        (ignore-errors (delete-file new-file))))))

(defun bob/gptel-tools--reformat-unified-diff (unified new-content)
  "Turn a `diff -U4' UNIFIED string into a pi-style numbered diff.
NEW-CONTENT is the full new file; its line count is used to size the
line-number column."
  (let* ((new-lines (length (split-string new-content "\n" nil)))
         (width (length (number-to-string (max 1 new-lines))))
         (pad (lambda (n)
                (format (format "%%%ds" width)
                        (if n (number-to-string n) ""))))
         (lines (split-string unified "\n"))
         (out '())
         old-line new-line
         (prev-end-old nil))
      (dolist (line lines)
        (cond
         ;; Skip file headers.
         ((or (string-prefix-p "--- " line)
              (string-prefix-p "+++ " line)
              (string-empty-p line))
          nil)
         ;; Hunk header: @@ -a,b +c,d @@
         ((string-match "\\`@@ -\\([0-9]+\\)\\(?:,[0-9]+\\)? \\+\\([0-9]+\\)\\(?:,[0-9]+\\)? @@"
                        line)
          (let ((new-start (string-to-number (match-string 2 line))))
            (when (and prev-end-old (> (string-to-number (match-string 1 line))
                                       (1+ prev-end-old)))
              (push (format " %s ..." (funcall pad nil)) out))
            (setq old-line (string-to-number (match-string 1 line))
                  new-line new-start)))
         ;; Added line.
         ((string-prefix-p "+" line)
          (push (format "+%s %s" (funcall pad new-line) (substring line 1)) out)
          (setq new-line (1+ new-line)))
         ;; Removed line.
         ((string-prefix-p "-" line)
          (push (format "-%s %s" (funcall pad old-line) (substring line 1)) out)
          (setq old-line (1+ old-line)))
         ;; Context line.
         ((string-prefix-p " " line)
          (push (format " %s %s" (funcall pad old-line) (substring line 1)) out)
          (setq old-line (1+ old-line)
                new-line (1+ new-line)))
         ;; "\ No newline at end of file" markers etc. -- ignore.
         (t nil))
        (when old-line (setq prev-end-old old-line)))
    (string-join (nreverse out) "\n")))

;;; read -----------------------------------------------------------------

(defun bob/gptel-tools--read (path &optional offset limit)
  "Return contents of PATH.
OFFSET is a 1-indexed starting line, LIMIT is a maximum line count.

If the output is truncated by the byte cap, or the user's LIMIT
stops short of the end of the file, appends a pi-style
`[Showing lines N-M of TOTAL. Use offset=X to continue.]' footer
so the LLM knows how to page."
  (let ((file (bob/gptel-tools--expand path)))
    (unless (file-readable-p file)
      (error "File not readable: %s" file))
    (with-temp-buffer
      (insert-file-contents file)
      (let* ((total-lines (count-lines (point-min) (point-max)))
             (start-line (max 1 (or offset 1)))
             (start (progn (goto-char (point-min))
                           (forward-line (1- start-line))
                           (point)))
             (want (and limit (> limit 0) limit))
             (end (if want
                      (progn (goto-char start)
                             (forward-line want)
                             (point))
                    (point-max)))
             (raw (buffer-substring-no-properties start end))
             (raw-bytes (string-bytes raw))
             (max-bytes bob/gptel-tools-output-max-bytes))
        (when (> start-line total-lines)
          (error "offset %d is beyond end of file (%d lines total)"
                 start-line total-lines))
        ;; If the byte cap will chop the output, find the last full line
        ;; that fits so we can give an accurate "Showing lines X-Y" footer.
        (let* ((count-content-lines
                (lambda (s)
                  ;; Number of "lines of content" the way a user
                  ;; counts them: trailing newline does not add a line.
                  (cond
                   ((string-empty-p s) 0)
                   ((string-suffix-p "\n" s)
                    (length (split-string s "\n" t)))
                   (t (1+ (cl-count ?\n s))))))
               (byte-truncated (> raw-bytes max-bytes))
               (shown raw)
               (shown-lines (funcall count-content-lines raw))
               (shown-end-line (+ start-line (max 0 (1- shown-lines)))))
          (when byte-truncated
            ;; Trim to max-bytes at the last newline so we don't cut a line.
            (let* ((head (substring raw 0 (min max-bytes (length raw))))
                   (last-nl (cl-position ?\n head :from-end t)))
              (setq shown (if last-nl (substring head 0 (1+ last-nl)) head)
                    shown-lines (funcall count-content-lines shown)
                    shown-end-line (+ start-line (max 0 (1- shown-lines))))))
          (cond
           (byte-truncated
            (format "%s\n\n[Showing lines %d-%d of %d (%d byte cap reached). Use offset=%d to continue.]"
                    shown start-line shown-end-line total-lines
                    max-bytes (1+ shown-end-line)))
           ((and want (< shown-end-line total-lines))
            (format "%s\n\n[Showing lines %d-%d of %d. Use offset=%d to continue.]"
                    shown start-line shown-end-line total-lines
                    (1+ shown-end-line)))
           (t shown)))))))

(gptel-make-tool
 :name "read"
 :function #'bob/gptel-tools--read
 :description "Read a file from disk and return its contents as text.
Supports optional line-based paging with OFFSET (1-indexed first line)
and LIMIT (number of lines).  Output is truncated at ~50KB."
 :args (list '(:name "path"
               :type string
               :description "Path to the file (relative or absolute).")
             '(:name "offset"
               :type integer
               :optional t
               :description "1-indexed line to start reading from.")
             '(:name "limit"
               :type integer
               :optional t
               :description "Maximum number of lines to read."))
 :category "pi")

;;; write ----------------------------------------------------------------

(defun bob/gptel-tools--write (path content)
  "Write CONTENT to PATH, creating parent directories as needed.
Returns a pi-style header plus a numbered diff against the previous
contents (or a \"(new file)\" notice if the file did not exist)."
  (let* ((file (bob/gptel-tools--expand path))
         (existed (file-exists-p file))
         (old (if existed
                  (with-temp-buffer (insert-file-contents file) (buffer-string))
                ""))
         (bytes (string-bytes content))
         (new-lines (length (split-string content "\n" nil))))
    (make-directory (file-name-directory file) t)
    (with-temp-file file
      (insert content))
    (if existed
        (let ((diff (bob/gptel-tools--format-diff old content)))
          (bob/gptel-tools--truncate
           (format "Wrote %s (%d bytes, %d lines) -- overwritten:\n%s"
                   file bytes new-lines
                   (if (string-empty-p diff) "(no changes)" diff))))
      (bob/gptel-tools--truncate
       (format "Wrote %s (%d bytes, %d lines) -- new file."
               file bytes new-lines)))))

(gptel-make-tool
 :name "write"
 :function #'bob/gptel-tools--write
 :description "Create or overwrite a file with the given content.
Parent directories are created automatically.  Use `edit' for in-place
modifications of an existing file."
 :args (list '(:name "path"
               :type string
               :description "Path to the file to write.")
             '(:name "content"
               :type string
               :description "Full file contents to write."))
 :category "pi"
 :confirm t)

;;; edit -----------------------------------------------------------------

(defun bob/gptel-tools--edit-field (entry plist-key &rest alist-keys)
  "Look up a string field in ENTRY, trying both plist and alist shapes."
  (cond
   ((and (listp entry) (keywordp (car entry)))
    (plist-get entry plist-key))
   ((listp entry)
    (cl-some (lambda (k) (alist-get k entry nil nil #'equal))
             (append alist-keys (list (symbol-name plist-key)))))))

(defun bob/gptel-tools--edit (path edits)
  "Apply EDITS to PATH as a single atomic transaction.
EDITS is a vector or list of plists/alists with `oldText'/`newText'
keys (or `old_text'/`new_text').  Each `oldText' must match exactly once
against the original file.  Two `oldText' regions may not overlap."
  (let* ((file (bob/gptel-tools--expand path))
         (edit-list (if (vectorp edits) (append edits nil) edits))
         (pairs
          (cl-loop
           for e in edit-list
           for old = (bob/gptel-tools--edit-field e :oldText 'oldText 'old_text)
           for new = (bob/gptel-tools--edit-field e :newText 'newText 'new_text)
           unless (and (stringp old) (stringp new))
           do (error "edit entry missing oldText/newText: %S" e)
           collect (cons old new))))
    (unless (file-readable-p file)
      (error "File not readable: %s" file))
    (let ((old-content (with-temp-buffer
                         (insert-file-contents file)
                         (buffer-string)))
          (new-content nil))
    (with-temp-buffer
      (insert-file-contents file)
      ;; Phase 1: validate all oldText strings match exactly once, and
      ;; find their positions.  We do this against the ORIGINAL buffer
      ;; (before any replacement) so later edits don't depend on the
      ;; outcome of earlier edits -- matching pi's semantics.
      (let ((hits
             (cl-loop
              for (old . _new) in pairs
              for start = (save-excursion
                            (goto-char (point-min))
                            (let ((count 0) first-pos)
                              (while (search-forward old nil t)
                                (cl-incf count)
                                (unless first-pos
                                  (setq first-pos (match-beginning 0))))
                              (cond
                               ((zerop count)
                                (error "oldText not found: %S"
                                       (truncate-string-to-width old 80)))
                               ((> count 1)
                                (error "oldText matches %d times (must be unique): %S"
                                       count (truncate-string-to-width old 80))))
                              first-pos))
              collect (list start (+ start (length old)) old))))
        ;; Phase 2: check non-overlap (pairwise, i < j).
        (cl-loop
         for tail on hits
         for (s1 e1 _) = (car tail)
         do (dolist (other (cdr tail))
              (pcase-let ((`(,s2 ,e2 ,_) other))
                (when (and (< s1 e2) (< s2 e1))
                  (error "overlapping edits at positions %d and %d"
                         s1 s2)))))
        ;; Phase 3: apply replacements from the bottom up so earlier
        ;; positions remain valid.
        (let ((plan (cl-mapcar #'cons hits pairs)))
          (setq plan (cl-sort plan #'> :key (lambda (x) (car (car x)))))
          (dolist (entry plan)
            (let* ((hit (car entry))
                   (pair (cdr entry))
                   (start (nth 0 hit))
                   (end (nth 1 hit))
                   (new (cdr pair)))
              (delete-region start end)
              (goto-char start)
              (insert new)))))
      (setq new-content (buffer-string))
      (write-region (point-min) (point-max) file))
    (let ((diff (bob/gptel-tools--format-diff old-content new-content)))
      (bob/gptel-tools--truncate
       (format "Applied %d edit%s to %s:\n%s"
               (length pairs) (if (= 1 (length pairs)) "" "s") file
               (if (string-empty-p diff) "(no net change)" diff)))))))

(gptel-make-tool
 :name "edit"
 :function #'bob/gptel-tools--edit
 :description "Edit a file by applying one or more exact string replacements.
Each entry in EDITS has an `oldText' that must match a unique region of
the original file, and a `newText' that replaces it verbatim.  All
edits in one call are applied atomically as a single transaction.

Matching is literal (no regex, no whitespace normalisation).  Prefer
batching multiple changes to the same file in one `edit' call rather
than calling `edit' multiple times.  Two edits may not have overlapping
`oldText' regions."
 :args (list '(:name "path"
               :type string
               :description "Path to the file to edit.")
             (list :name "edits"
                   :type 'array
                   :description "List of edits to apply in one transaction."
                   :items '(:type object
                            :properties
                            (:oldText (:type string
                                       :description "Exact text to find.  Must match once.")
                             :newText (:type string
                                       :description "Replacement text."))
                            :required ["oldText" "newText"])))
 :category "pi"
 :confirm t)

;;; ls -------------------------------------------------------------------

(defun bob/gptel-tools--ls (&optional path limit)
  "List entries of PATH (defaults to `default-directory').
LIMIT caps the number of entries returned."
  (let* ((dir (bob/gptel-tools--expand (or path default-directory)))
         (limit (or limit bob/gptel-tools-ls-default-limit))
         (entries (seq-remove
                   (lambda (e) (member e '("." "..")))
                   (directory-files dir nil nil t)))
         (total (length entries))
         (shown (seq-take entries limit))
         (lines (mapcar (lambda (e)
                          (let ((full (expand-file-name e dir)))
                            (format "%s%s"
                                    e
                                    (if (file-directory-p full) "/" ""))))
                        shown))
         (body (if lines (string-join lines "\n") "(empty)")))
    (bob/gptel-tools--truncate
     (if (> total limit)
         (format "%s\n[... %d more entries truncated, limit=%d ...]"
                 body (- total limit) limit)
       body))))

(gptel-make-tool
 :name "ls"
 :function #'bob/gptel-tools--ls
 :description "List entries in a directory.  Directories are marked with a
trailing slash.  Defaults to the current working directory."
 :args (list '(:name "path"
               :type string
               :optional t
               :description "Directory to list.  Defaults to cwd.")
             '(:name "limit"
               :type integer
               :optional t
               :description "Maximum number of entries to return."))
 :category "pi")

;;; find -----------------------------------------------------------------

(defcustom bob/gptel-tools-find-timeout 30
  "Timeout (seconds) for a single `find' tool call."
  :type 'integer
  :group 'bob-gptel-tools)

(defun bob/gptel-tools--find (callback pattern &optional path limit)
  "Find files matching glob PATTERN under PATH.
Uses `fd' if available, falling back to POSIX `find'.  LIMIT caps the
number of results.

Async: CALLBACK is called with the formatted result string."
  (let* ((dir (bob/gptel-tools--expand (or path default-directory)))
         (limit (or limit bob/gptel-tools-find-default-limit))
         (fd (or (executable-find "fd") (executable-find "fdfind")))
         (program (or fd "find"))
         (args (if fd
                   (list "--glob" "--hidden" "--max-results"
                         (number-to-string limit)
                         pattern dir)
                 ;; POSIX `find': no built-in result cap; emulate by
                 ;; letting the caller truncate output.  `-name' keeps
                 ;; case sensitivity consistent with fd's glob.
                 (list dir "-name" pattern)))
         (wrapped
          (lambda (raw)
            (let* ((body (if (string-prefix-p "[" raw)
                             ;; Killed or timed out -- keep helper's marker.
                             raw
                           (let* ((out (car (split-string raw "\n\\[exit: " t)))
                                  (trimmed (string-trim-right (or out "")))
                                  (count (if (string-empty-p trimmed) 0
                                           (length (split-string trimmed "\n")))))
                             (if (zerop count)
                                 (format "No matches for %s under %s" pattern dir)
                               (format "%s\n[%d result%s]"
                                       trimmed count
                                       (if (= 1 count) "" "s")))))))
              (funcall callback body)))))
    (bob/gptel-tools--run-async-process
     wrapped
     :tool "find"
     :label (format "%S in %s" pattern dir)
     :program program
     :args args
     :timeout bob/gptel-tools-find-timeout)))

(gptel-make-tool
 :name "find"
 :function #'bob/gptel-tools--find
 :async t
 :description "Find files by name using a glob pattern.
Uses `fd' if available, otherwise POSIX `find'.  The pattern uses
shell-style wildcards (e.g. \"*.el\", \"**/test_*.py\")."
 :args (list '(:name "pattern"
               :type string
               :description "Glob pattern to match filenames against.")
             '(:name "path"
               :type string
               :optional t
               :description "Root directory to search from.  Defaults to cwd.")
             '(:name "limit"
               :type integer
               :optional t
               :description "Maximum number of results to return."))
 :category "pi")

;;; grep -----------------------------------------------------------------

(defcustom bob/gptel-tools-grep-timeout 30
  "Timeout (seconds) for a single `grep' tool call."
  :type 'integer
  :group 'bob-gptel-tools)

(cl-defun bob/gptel-tools--grep (callback pattern &optional path glob
                                          ignore-case literal context limit)
  "Search for PATTERN in files under PATH using ripgrep.
GLOB restricts to matching filenames.  IGNORE-CASE, LITERAL, CONTEXT,
and LIMIT map onto `rg' flags.

Async: CALLBACK is called with the formatted result string."
  (unless (executable-find "rg")
    (funcall callback "grep tool requires ripgrep (rg) on PATH")
    (cl-return-from bob/gptel-tools--grep nil))
  (let* ((dir (bob/gptel-tools--expand (or path default-directory)))
         (limit (or limit bob/gptel-tools-grep-default-limit))
         (args (delq nil
                     (list "--line-number"
                           "--color=never"
                           (and ignore-case "--ignore-case")
                           (and literal "--fixed-strings")
                           (when (and context (> context 0))
                             (format "--context=%d" context))
                           (when (and glob (not (string-empty-p glob)))
                             (format "--glob=%s" glob))
                           (format "--max-count=%d" limit)
                           "--"
                           pattern
                           dir)))
         (wrapped
          (lambda (raw)
            (let ((body
                   (if (string-prefix-p "[" raw)
                       raw
                     ;; Split off our "[exit: N]" suffix to recover
                     ;; rg's real exit status.
                     (let* ((parts (split-string raw "\n\\[exit: " t))
                            (out (string-trim-right (or (car parts) "")))
                            (status-str (cadr parts))
                            (status (and status-str
                                         (string-to-number status-str))))
                       (cond
                        ((eql status 0) out)
                        ((eql status 1)
                         (format "No matches for %S under %s" pattern dir))
                        (t (format "rg exited %s\n%s" status out)))))))
              (funcall callback body)))))
    (bob/gptel-tools--run-async-process
     wrapped
     :tool "grep"
     :label (format "%S in %s" pattern dir)
     :program "rg"
     :args args
     :timeout bob/gptel-tools-grep-timeout)))

(gptel-make-tool
 :name "grep"
 :function #'bob/gptel-tools--grep
 :async t
 :description "Search file contents for a regex PATTERN using ripgrep.
Returns matching lines with filenames and line numbers."
 :args (list '(:name "pattern"
               :type string
               :description "Regex pattern to search for.")
             '(:name "path"
               :type string
               :optional t
               :description "Directory or file to search.  Defaults to cwd.")
             '(:name "glob"
               :type string
               :optional t
               :description "Glob restricting which files are searched (e.g. \"*.el\").")
             '(:name "ignoreCase"
               :type boolean
               :optional t
               :description "Case-insensitive matching.")
             '(:name "literal"
               :type boolean
               :optional t
               :description "Treat pattern as a literal string, not a regex.")
             '(:name "context"
               :type integer
               :optional t
               :description "Number of lines of context around each match.")
             '(:name "limit"
               :type integer
               :optional t
               :description "Maximum number of matches per file."))
 :category "pi")

;;; bash -----------------------------------------------------------------

(defun bob/gptel-tools--bash (callback command &optional timeout)
  "Run shell COMMAND via bash -lc.
TIMEOUT defaults to `bob/gptel-tools-bash-timeout' seconds.  Working
directory is the current `default-directory' (which is typically the
cwd of the buffer from which gptel was invoked).

Async: CALLBACK is called with the formatted result string."
  (let* ((timeout (or timeout bob/gptel-tools-bash-timeout))
         (cwd default-directory)
         (wrapped
          (lambda (raw)
            (funcall callback
                     (format "$ %s\n[cwd: %s]\n%s"
                             command cwd raw)))))
    (bob/gptel-tools--run-async-process
     wrapped
     :tool "bash"
     :label command
     :program "bash"
     :args (list "-lc" command)
     :timeout timeout)))

(gptel-make-tool
 :name "bash"
 :function #'bob/gptel-tools--bash
 :async t
 :description "Run a shell command via `bash -lc' and return combined output.
Output is captured, truncated at ~50KB, and suffixed with the exit
status.  The command runs in the current working directory."
 :args (list '(:name "command"
               :type string
               :description "Shell command to execute.")
             '(:name "timeout"
               :type integer
               :optional t
               :description "Timeout in seconds."))
 :category "pi"
 :confirm t)

;;; jina_reader -------------------------------------------------------

;; Mirrors the `jina_reader' tool from ~/.pi/agent/extensions/jina-reader.ts.
;; Fetches a URL through Jina's r.jina.ai front-end, which returns the
;; page as clean markdown -- handy for API docs and reference pages.

(defcustom bob/gptel-tools-jina-api-key nil
  "Optional Jina API key for r.jina.ai.
If set, sent as `Authorization: Bearer KEY' for higher rate limits.
Can be a string or a function returning a string (e.g. looked up from
`auth-source').  Pi's own extension does not use a key."
  :type '(choice (const :tag "None" nil) string function)
  :group 'bob-gptel-tools)

(defcustom bob/gptel-tools-jina-timeout 60
  "Timeout (seconds) for a single Jina Reader request."
  :type 'integer
  :group 'bob-gptel-tools)

(defun bob/gptel-tools--jina-api-key ()
  "Return the Jina API key as a string, or nil."
  (let ((v bob/gptel-tools-jina-api-key))
    (cond ((functionp v) (funcall v))
          ((stringp v) v)
          (t nil))))

(cl-defun bob/gptel-tools--jina-reader (callback url)
  "Fetch URL via https://r.jina.ai and call CALLBACK with the response body.

Calls CALLBACK with an error string on HTTP >= 400, network
failure, or timeout (`bob/gptel-tools-jina-timeout').

Async: uses `url-retrieve', not `url-retrieve-synchronously'."
  (unless (and (stringp url) (string-match-p "\\`https?://" url))
    (funcall callback
             (format "jina_reader: URL must start with http:// or https://, got %S" url))
    (cl-return-from bob/gptel-tools--jina-reader nil))
  (let* ((jina-url (concat "https://r.jina.ai/" url))
         (url-request-method "GET")
         (key (bob/gptel-tools--jina-api-key))
         (url-request-extra-headers
          (append
           '(("Accept" . "text/plain"))
           (when key `(("Authorization" . ,(concat "Bearer " key))))))
         (done (list nil))               ;boxed flag: response arrived
         (timer nil)
         (fetch-buffer nil))
    (cl-labels
        ((finish (msg)
           (unless (car done)
             (setcar done t)
             (when timer (cancel-timer timer))
             (when (buffer-live-p fetch-buffer) (kill-buffer fetch-buffer))
             (funcall callback (bob/gptel-tools--truncate msg))))
         (on-response (status)
           (let ((err (plist-get status :error)))
             (cond
              (err (finish (format "jina_reader: network error: %S" err)))
              (t
               (condition-case e
                   (progn
                     (goto-char (point-min))
                     (let ((http-status
                            (if (re-search-forward
                                 "^HTTP/[0-9.]+ \\([0-9]+\\)"
                                 (line-end-position) t)
                                (string-to-number (match-string 1))
                              0)))
                       (goto-char (point-min))
                       (unless (re-search-forward "^\r?\n" nil t)
                         (error "no headers terminator"))
                       (let ((body (buffer-substring-no-properties
                                    (point) (point-max))))
                         (cond
                          ((>= http-status 400)
                           (finish (format "jina_reader: HTTP %d: %s"
                                           http-status
                                           (substring body 0
                                                      (min 500
                                                           (length body))))))
                          ((zerop http-status)
                           (finish (format "jina_reader: malformed response from %s"
                                           jina-url)))
                          (t (finish body)))))
                     (setq fetch-buffer (current-buffer)))
                 (error (finish (format "jina_reader: %S" e)))))))))
      (setq fetch-buffer (url-retrieve jina-url #'on-response nil t t))
      (when (and bob/gptel-tools-jina-timeout
                 (> bob/gptel-tools-jina-timeout 0))
        (setq timer
              (run-at-time
               bob/gptel-tools-jina-timeout nil
               (lambda ()
                 (finish (format "jina_reader: request to %s timed out after %ds"
                                 jina-url
                                 bob/gptel-tools-jina-timeout))))))
      fetch-buffer)))

(gptel-make-tool
 :name "jina_reader"
 :function #'bob/gptel-tools--jina-reader
 :async t
 :description "Fetch a web page and return its content as clean markdown using Jina Reader.
Useful for reading API documentation, blog posts, or any web page."
 :args (list '(:name "url"
               :type string
               :description "Full URL to fetch (e.g. https://docs.example.com/api)"))
 :category "pi")

;;; elisp_eval ----------------------------------------------------------

;; Adapted from karthink/gptel-agent's `Eval' tool:
;; https://github.com/karthink/gptel-agent/blob/master/gptel-agent-tools.el

(defun bob/gptel-tools--elisp-eval (expression)
  "Evaluate EXPRESSION (a string containing a single elisp sexp).
Returns the result formatted with %S, plus any captured STDOUT from
`print', `prin1', or `princ' during evaluation."
  (let ((standard-output (generate-new-buffer " *bob-gptel-elisp-eval*"))
        (result nil) (output nil))
    (unwind-protect
        (condition-case err
            (progn
              (setq result (eval (read expression) t))
              (when (> (buffer-size standard-output) 0)
                (setq output (with-current-buffer standard-output
                               (buffer-string))))
              (concat
               (format "Result:\n%S" result)
               (and output (format "\n\nSTDOUT:\n%s" output))))
          ((error user-error)
           (concat
            (format "Error: eval failed with error %S: %S"
                    (car err) (cdr err))
            (and output (format "\n\nSTDOUT:\n%s" output)))))
      (kill-buffer standard-output))))

(gptel-make-tool
 :name "elisp_eval"
 :function #'bob/gptel-tools--elisp-eval
 :description "Evaluate Elisp EXPRESSION and return result and any printed output.

EXPRESSION can be anything to evaluate.  It can be a function call, a
variable, a quasi-quoted expression.  The only requirement is that only
the first sexp will be read and evaluated, so if you need to evaluate
multiple expressions, make one call per expression.  Do not combine
expressions using progn etc.  Just go expression by expression and try
to make standalone single expressions.

Instead of saying \"I can't calculate that\" etc, use this tool to
evaluate the result.

The return value is formated to a string using %S, so a string will be
returned as an escaped embedded string and literal forms will be
compatible with `read' where possible.  Some forms have no printed
representation that can be read and will be represented with
#<hash-notation> instead.

Output from `print`, `prin1`, and `princ` is captured and returned as STDOUT.
Use `print` for diagnostic output, not `message` (which goes to *Messages* buffer
and is not captured).

You can use this to quickly change a user setting, check a variable, or
demonstrate something to the user."
 :args '(( :name "expression"
           :type string
           :description "A single elisp sexp to evaluate."))
 :category "emacs"
 :confirm t
 :include t)

;;; Default selection ----------------------------------------------------

(defconst bob/gptel-tools-default-names
  '("read" "write" "edit" "ls" "find" "grep" "bash"
    "jina_reader" "elisp_eval")
  "Tool names installed by `bob/gptel-tools-default-set'.")

(defun bob/gptel-tools-default-set ()
  "Set `gptel-tools' to this module's default tool set."
  (interactive)
  (setq gptel-use-tools t
        gptel-tools
        (delq nil
              (mapcar (lambda (name) (gptel-get-tool name))
                      bob/gptel-tools-default-names)))
  (when (called-interactively-p 'interactive)
    (message "gptel-tools: %s"
             (mapconcat #'gptel-tool-name gptel-tools ", "))))

;;; System prompt -------------------------------------------------------

;; Adapted from pi-coding-agent's buildSystemPrompt, keeping the same
;; structure and per-tool snippets but tailored for gptel inside Emacs.
;; Pi's source: dist/core/system-prompt.js.

(defconst bob/gptel-tools--prompt-snippets
  '(("read"       . "Read file contents")
    ("write"      . "Create or overwrite files")
    ("edit"       . "Make precise file edits with exact text replacement, including multiple disjoint edits in one call")
    ("ls"         . "List directory contents")
    ("find"       . "Find files by glob pattern")
    ("grep"       . "Search file contents for patterns (ripgrep)")
    ("bash"       . "Execute bash commands (ls, grep, find, etc.)")
    ("jina_reader" . "Fetch a URL as clean readable markdown via Jina Reader")
    ("elisp_eval" . "Evaluate Emacs Lisp in the running Emacs and return the result"))
  "One-line description for each tool, used in the system prompt.
Mirrors pi's per-tool `promptSnippet' fields.")

(defcustom bob/gptel-tools-extra-guidelines nil
  "Extra guideline strings appended after the built-in ones.
Each entry becomes a bullet under \"Guidelines:\" in the system prompt."
  :type '(repeat string)
  :group 'bob-gptel-tools)

(defun bob/gptel-tools--selected-tool-names ()
  "Return the names of currently selected gptel tools as strings.
Falls back to `bob/gptel-tools-default-names' if none are selected."
  (or (and gptel-tools
           (mapcar #'gptel-tool-name gptel-tools))
      bob/gptel-tools-default-names))

(defconst bob/gptel-tools--tool-guidelines
  ;; Per-tool guidelines copied verbatim from pi's own source:
  ;;   dist/core/tools/{read,write,edit}.js -> promptGuidelines
  ;; Tools not listed here (bash, find, grep, ls) carry no per-tool
  ;; guidelines in pi either.  The `elisp_eval' entry is our own
  ;; addition since pi has no equivalent tool.
  '(("read"
     "Use read to examine files instead of cat or sed.")
    ("write"
     "Use write only for new files or complete rewrites.")
    ("edit"
     "Use edit for precise changes (edits[].oldText must match exactly)"
     "When changing multiple separate locations in one file, use one edit call with multiple entries in edits[] instead of multiple edit calls"
     "Each edits[].oldText is matched against the original file, not after earlier edits are applied. Do not emit overlapping or nested edits. Merge nearby changes into one edit."
     "Keep edits[].oldText as small as possible while still being unique in the file. Do not pad with large unchanged regions.")
    ("jina_reader" ; verbatim from ~/.pi/agent/extensions/jina-reader.ts
     "Use jina_reader when you need to fetch and read web page content, especially API docs or reference material.")
    ("elisp_eval" ; not from pi -- our own addition
     "Use elisp_eval to inspect or change Emacs state (buffers, variables, commands). Evaluate one sexp per call."))
  "Per-tool guidelines, keyed by tool name.
The `read', `write', and `edit' entries are verbatim from pi's source.")

(defun bob/gptel-tools-system-prompt ()
  "Return a pi-inspired system prompt tailored for the active gptel tools.

The prompt lists whichever tools are currently selected in
`gptel-tools' (falling back to the module defaults) with their
one-line descriptions, then adds pi's own guidelines for the
file-exploration and editing tools, any extra user guidelines, and
finally the standard \"Be concise\" / \"Show file paths\" closers,
plus the current date and working directory.

The guideline wording is verbatim from pi wherever possible so the
LLM can reuse the same mental model across pi and gptel."
  (let* ((names (bob/gptel-tools--selected-tool-names))
         (has (lambda (n) (member n names)))
         (tool-lines
          (mapconcat
           (lambda (name)
             (let ((snippet (alist-get name bob/gptel-tools--prompt-snippets
                                       nil nil #'equal)))
               (format "- %s: %s" name (or snippet "(no description)"))))
           names "\n"))
         (guidelines '())
         (add (lambda (g) (unless (member g guidelines)
                            (push g guidelines)))))
    ;; File-exploration guideline -- verbatim from pi's system-prompt.js.
    ;; `rg' respects .gitignore by default; `fd' respects it too (we
    ;; deliberately do not pass --no-ignore-vcs), so pi's wording
    ;; applies here.  The POSIX `find' fallback does NOT honour
    ;; .gitignore, so if you run without `fd' installed the "respects
    ;; .gitignore" promise only applies to `grep'.
    (cond
     ((and (funcall has "bash")
           (not (or (funcall has "grep") (funcall has "find") (funcall has "ls"))))
      (funcall add "Use bash for file operations like ls, rg, find"))
     ((and (funcall has "bash")
           (or (funcall has "grep") (funcall has "find") (funcall has "ls")))
      (funcall add "Prefer grep/find/ls tools over bash for file exploration (faster, respects .gitignore)")))
    ;; Per-tool guidelines (verbatim from pi where applicable).
    (dolist (name names)
      (dolist (g (cdr (assoc name bob/gptel-tools--tool-guidelines)))
        (funcall add g)))
    ;; User-supplied extras.
    (dolist (g bob/gptel-tools-extra-guidelines)
      (let ((trimmed (string-trim g)))
        (unless (string-empty-p trimmed) (funcall add trimmed))))
    ;; Closers -- verbatim from pi.
    (funcall add "Be concise in your responses")
    (funcall add "Show file paths clearly when working with files")
    (setq guidelines (nreverse guidelines))
    (format "You are an expert coding assistant operating inside Emacs via gptel.
You help the user by reading files, executing commands, editing code,
writing new files, and introspecting Emacs itself.

Available tools:
%s

In addition to the tools above, you may have access to other custom tools
depending on the buffer's configuration.

Guidelines:
%s

Current date: %s
Current working directory: %s"
            tool-lines
            (mapconcat (lambda (g) (format "- %s" g)) guidelines "\n")
            (format-time-string "%Y-%m-%d")
            (abbreviate-file-name default-directory))))

;;; Auto-fold tool blocks ----------------------------------------------

(defcustom bob/gptel-tools-fold-by-default
  '("read" "ls" "find" "grep" "bash" "jina_reader" "write")
  "Tool names whose result block should start folded in the gptel buffer.

When a response lands, each tool block whose tool name is in this list
is folded so it shows only a one-line summary.  The user can expand it
with TAB (org-mode) or by clicking the fence (markdown).

Defaults target high-volume tools where the raw output is rarely
useful to eyeball -- including `write', whose full new-file dump is
usually long and not interesting.  `edit' is NOT folded because its
diff is typically small and is the main thing the user wants to see."
  :type '(repeat string)
  :group 'bob-gptel-tools)

(defun bob/gptel-tools--fold-block-at (beg end)
  "Fold the block spanning BEG..END via `org-hide-block-toggle' or an overlay.
BEG must be at the start of the opening fence line; END just past the
closing fence line."
  (save-excursion
    (goto-char beg)
    (cond
     ((derived-mode-p 'org-mode)
      ;; `org-hide-block-toggle' on the #+begin_tool line.
      (ignore-errors (org-hide-block-toggle t)))
     (t
      ;; Markdown / plain: mirror `gptel-markdown-cycle-block' -- an
      ;; invisible overlay from the end of the opening fence line to
      ;; the end of the closing fence line, with a "..." marker.
      (end-of-line)
      (let ((ov-start (point))
            (ov-end (save-excursion (goto-char end) (line-end-position 0))))
        (when (> ov-end ov-start)
          (let ((ov (make-overlay ov-start ov-end)))
            (overlay-put ov 'evaporate t)
            (overlay-put ov 'invisible t)
            (overlay-put ov 'before-string
                         (propertize "..." 'face 'shadow)))))))))

(defun bob/gptel-tools--fold-tool-blocks (beg end)
  "Scan BEG..END for freshly-inserted tool blocks and fold the ones
whose tool name is in `bob/gptel-tools-fold-by-default'.

Runs on `gptel-post-response-functions'."
  (save-excursion
    (save-match-data
      (goto-char beg)
      (let* ((org-p (derived-mode-p 'org-mode))
             ;; Opening-fence regex: captures the tool name that gptel
             ;; writes as `#+begin_tool (NAME ...)' or `\`\`\` tool (NAME ...)'.
             (open-re (if org-p
                          "^#\\+begin_tool +(\\([^ )]+\\)"
                        "^``` tool +(\\([^ )]+\\)"))
             (close-re (if org-p
                           "^#\\+end_tool\\b"
                         "^``` *$")))
        (while (re-search-forward open-re end t)
          (let ((block-beg (match-beginning 0))
                (name (match-string-no-properties 1)))
            (when (member name bob/gptel-tools-fold-by-default)
              (let ((block-end (save-excursion
                                 (if (re-search-forward close-re end t)
                                     (1+ (match-end 0))
                                   end))))
                (bob/gptel-tools--fold-block-at block-beg block-end)))))))))

(defun bob/gptel-tools--unfold-reasoning-blocks (beg end)
  "Force any `#+begin_reasoning' blocks in BEG..END to stay expanded.

Something in the org-mode startup path (often
`org-hide-block-startup' or a project-wide fold-on-insert rule)
likes to collapse freshly-inserted `#+begin_reasoning' blocks.
We undo that here by removing `org-hide-block' invisibility
overlays that intersect a reasoning block."
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (save-match-data
        (goto-char beg)
        (while (re-search-forward "^#\\+begin_reasoning\\b" end t)
          (let* ((block-beg (match-beginning 0))
                 (block-end (save-excursion
                              (if (re-search-forward "^#\\+end_reasoning\\b"
                                                     end t)
                                  (match-end 0)
                                end))))
            (dolist (ov (overlays-in block-beg block-end))
              (when (eq (overlay-get ov 'invisible) 'org-hide-block)
                (delete-overlay ov)))))))))

(defun bob/gptel-tools--unfold-reasoning-stream ()
  "Force reasoning blocks expanded during streaming.
Runs on `gptel-post-stream-hook', which fires after each chunk."
  (bob/gptel-tools--unfold-reasoning-blocks (point-min) (point-max)))

(defun bob/gptel-tools-fold-hook-install ()
  "Install the fold/unfold hooks.

Runs two things after each response lands
(on `gptel-post-response-functions'):
- tool blocks listed in `bob/gptel-tools-fold-by-default' are folded;
- any `#+begin_reasoning' blocks are force-expanded.

Also force-expands reasoning blocks during streaming
(on `gptel-post-stream-hook') so they do not visibly collapse
mid-stream if org-mode decides to hide them."
  (add-hook 'gptel-post-response-functions
            #'bob/gptel-tools--fold-tool-blocks)
  (add-hook 'gptel-post-response-functions
            #'bob/gptel-tools--unfold-reasoning-blocks)
  (add-hook 'gptel-post-stream-hook
            #'bob/gptel-tools--unfold-reasoning-stream))

(defun bob/gptel-tools-fold-hook-remove ()
  "Remove the fold/unfold hooks."
  (interactive)
  (remove-hook 'gptel-post-response-functions
               #'bob/gptel-tools--fold-tool-blocks)
  (remove-hook 'gptel-post-response-functions
               #'bob/gptel-tools--unfold-reasoning-blocks)
  (remove-hook 'gptel-post-stream-hook
               #'bob/gptel-tools--unfold-reasoning-stream))

;;; ----------------------------------------------------------------------

(defun bob/gptel-tools-install-system-prompt ()
  "Install the pi-inspired system prompt as a gptel directive.
Adds `pi' to `gptel-directives' and sets it as the active
`gptel--system-message' so new gptel buffers use it."
  (interactive)
  (let ((key 'pi))
    (setf (alist-get key gptel-directives) #'bob/gptel-tools-system-prompt)
    (setq-default gptel--system-message (bob/gptel-tools-system-prompt))
    (when (called-interactively-p 'interactive)
      (message "gptel directive `pi' installed and set as default"))))

(provide 'bob-gptel-tools)
;;; bob-gptel-tools.el ends here
