;;; magit-ci.el --- Show GitHub CI status in magit-status -*- lexical-binding: t; -*-

;; Adds an asynchronous "CI" section to `magit-status' for GitHub
;; repositories, using the `gh' CLI.  Results are cached per
;; (repo . sha) and never block the status buffer.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'magit)
(require 'magit-section)

(defgroup bob/magit-ci nil
  "Asynchronous GitHub CI status section for `magit-status'."
  :group 'magit-extensions)

(defcustom bob/magit-ci-gh-program (or (executable-find "gh") "gh")
  "Path to the GitHub CLI."
  :type 'string
  :group 'bob/magit-ci)

(defcustom bob/magit-ci-cache-ttl 30
  "Seconds to keep a successful CI result cached for the same SHA.
Refresh requests for the same SHA within this window are ignored."
  :type 'integer
  :group 'bob/magit-ci)

(defcustom bob/magit-ci-max-checks-shown 30
  "Soft cap on how many individual checks are shown when expanded."
  :type 'integer
  :group 'bob/magit-ci)

(defface bob/magit-ci-pass
  '((t :inherit magit-process-ok))
  "Face for passing checks." :group 'bob/magit-ci)
(defface bob/magit-ci-fail
  '((t :inherit magit-process-ng))
  "Face for failing checks." :group 'bob/magit-ci)
(defface bob/magit-ci-pending
  '((t :inherit magit-signature-expired))
  "Face for pending/in-progress checks." :group 'bob/magit-ci)
(defface bob/magit-ci-skip
  '((t :inherit magit-dimmed))
  "Face for skipped checks." :group 'bob/magit-ci)
(defface bob/magit-ci-cancel
  '((t :inherit magit-dimmed :slant italic))
  "Face for cancelled checks." :group 'bob/magit-ci)
(defface bob/magit-ci-heading
  '((t :inherit magit-section-heading))
  "Face for the CI section heading label." :group 'bob/magit-ci)

;; ---------------------------------------------------------------------------
;; State

(cl-defstruct bob/magit-ci-entry
  status     ; symbol: loading | ok | error | none
  bucket     ; aggregate bucket: pass | fail | pending | skip | none
  checks     ; list of plists (:name :bucket :state :workflow :link)
  fetched-at ; float-time when last set
  message)   ; human string for error/none cases

(defvar bob/magit-ci--cache (make-hash-table :test 'equal)
  "Hash table keyed by (REPO-ROOT . SHA) -> `bob/magit-ci-entry'.")

(defvar bob/magit-ci--inflight (make-hash-table :test 'equal)
  "Hash table keyed by (REPO-ROOT . SHA) -> process while a fetch is running.")

;; ---------------------------------------------------------------------------
;; Helpers

(defun bob/magit-ci--github-remote-p ()
  "Return non-nil if origin remote is on github.com."
  (when-let* ((url (or (magit-get "remote" "origin" "url")
                       (magit-get "remote" "upstream" "url"))))
    (string-match-p "github\\.com" url)))

(defun bob/magit-ci--key ()
  "Return cache key (REPO-ROOT . HEAD-SHA) for current repo, or nil."
  (when-let* ((root (magit-toplevel))
              (sha  (magit-rev-parse "HEAD")))
    (cons (file-truename root) sha)))

(defun bob/magit-ci--bucket-face (bucket)
  (pcase bucket
    ('pass    'bob/magit-ci-pass)
    ('fail    'bob/magit-ci-fail)
    ('pending 'bob/magit-ci-pending)
    ('skip    'bob/magit-ci-skip)
    ('cancel  'bob/magit-ci-cancel)
    (_        'bob/magit-ci-pending)))

(defun bob/magit-ci--bucket-icon (bucket)
  (pcase bucket
    ('pass    "✓")
    ('fail    "✗")
    ('pending "●")
    ('skip    "–")
    ('cancel  "–")
    (_        "?")))

(defun bob/magit-ci--aggregate-bucket (checks)
  "Return aggregate bucket symbol from CHECKS list."
  (cond
   ((null checks) 'none)
   ((seq-some (lambda (c) (eq (plist-get c :bucket) 'fail)) checks)    'fail)
   ((seq-some (lambda (c) (eq (plist-get c :bucket) 'pending)) checks) 'pending)
   ((seq-some (lambda (c) (eq (plist-get c :bucket) 'pass)) checks)    'pass)
   (t 'skip)))

(defun bob/magit-ci--parse-bucket (s)
  (pcase (and s (downcase s))
    ("pass"    'pass)
    ("fail"    'fail)
    ("pending" 'pending)
    ("skipping" 'skip)
    ("skip"    'skip)
    ("cancel"  'cancel)
    (_         'pending)))

(defun bob/magit-ci--parse-pr-checks (json-string)
  "Parse `gh pr checks --json …' output.  Return list of plists."
  (let* ((json-array-type 'list)
         (json-object-type 'alist)
         (json-key-type 'string)
         (data (json-read-from-string json-string)))
    (mapcar (lambda (c)
              (list :name     (or (cdr (assoc "name" c)) "")
                    :workflow (or (cdr (assoc "workflow" c)) "")
                    :state    (or (cdr (assoc "state" c)) "")
                    :bucket   (bob/magit-ci--parse-bucket (cdr (assoc "bucket" c)))
                    :link     (or (cdr (assoc "link" c)) "")))
            data)))

(defun bob/magit-ci--commit-conclusion->bucket (status conclusion)
  "Map check-runs API STATUS/CONCLUSION strings to a bucket symbol."
  (cond
   ((not (equal status "completed")) 'pending)
   ((equal conclusion "success")     'pass)
   ((member conclusion '("failure" "timed_out" "action_required" "startup_failure")) 'fail)
   ((member conclusion '("skipped" "neutral")) 'skip)
   ((equal conclusion "cancelled")   'cancel)
   (t 'pending)))

(defun bob/magit-ci--parse-commit-checks (json-string)
  "Parse output of `gh api repos/.../commits/SHA/check-runs'."
  (let* ((json-array-type 'list)
         (json-object-type 'alist)
         (json-key-type 'string)
         (data (json-read-from-string json-string))
         (runs (cdr (assoc "check_runs" data))))
    (mapcar (lambda (r)
              (let* ((status     (cdr (assoc "status" r)))
                     (conclusion (cdr (assoc "conclusion" r)))
                     (bucket     (bob/magit-ci--commit-conclusion->bucket status conclusion)))
                (list :name     (or (cdr (assoc "name" r)) "")
                      :workflow ""
                      :state    (or conclusion status "")
                      :bucket   bucket
                      :link     (or (cdr (assoc "html_url" r)) ""))))
            runs)))

;; ---------------------------------------------------------------------------
;; Async fetch

(defun bob/magit-ci--refresh-status-buffers (repo-root)
  "Refresh any visible magit-status buffers for REPO-ROOT.

Avoids shelling out to git for every buffer: in a `magit-status-mode'
buffer, `default-directory' is already the toplevel, so we just compare
truenames. We also check `get-buffer-window' before any other work so
hidden buffers cost effectively nothing."
  (let ((root (file-truename repo-root)))
    (dolist (buf (buffer-list))
      (when (and (buffer-live-p buf)
                 (get-buffer-window buf t))
        (with-current-buffer buf
          (when (and (derived-mode-p 'magit-status-mode)
                     (string= (file-truename default-directory) root))
            (let ((inhibit-message t))
              (magit-refresh-buffer))))))))

(defun bob/magit-ci--store (key entry)
  (puthash key entry bob/magit-ci--cache))

(defun bob/magit-ci--make-sentinel (key repo-root stdout-buf stderr-buf fallback-fn)
  "Return a process sentinel for the gh fetch identified by KEY."
  (lambda (proc event)
    (when (memq (process-status proc) '(exit signal))
      (remhash key bob/magit-ci--inflight)
      (let* ((exit  (process-exit-status proc))
             (out   (with-current-buffer stdout-buf (buffer-string)))
             (err   (with-current-buffer stderr-buf (buffer-string)))
             (entry nil))
        (ignore event)
        (unwind-protect
            (cond
             ;; gh exits 8 when checks are still pending — output is valid JSON.
             ((and (memq exit '(0 8))
                   (> (length (string-trim out)) 0)
                   (string-prefix-p "[" (string-trim out)))
              (let ((checks (ignore-errors (bob/magit-ci--parse-pr-checks out))))
                (setq entry
                      (make-bob/magit-ci-entry
                       :status     'ok
                       :bucket     (bob/magit-ci--aggregate-bucket checks)
                       :checks     checks
                       :fetched-at (float-time)))))
             ;; "no pull requests found" -> try commit-level fallback.
             ((and (not (zerop exit))
                   (string-match-p "no pull requests found" err)
                   fallback-fn)
              (funcall fallback-fn)
              (setq entry :delegated))
             ;; Genuine error.
             (t
              (setq entry
                    (make-bob/magit-ci-entry
                     :status     'error
                     :bucket     'none
                     :checks     nil
                     :fetched-at (float-time)
                     :message    (string-trim
                                  (if (> (length err) 0) err
                                    (format "gh exited with %s" exit)))))))
          (kill-buffer stdout-buf)
          (kill-buffer stderr-buf))
        (unless (eq entry :delegated)
          (when entry (bob/magit-ci--store key entry))
          (bob/magit-ci--refresh-status-buffers repo-root))))))

(defun bob/magit-ci--start-process (key repo-root args sentinel)
  "Start gh with ARGS in REPO-ROOT, attach SENTINEL.  Return process."
  (let* ((default-directory repo-root)
         (stdout (generate-new-buffer " *magit-ci-out*"))
         (stderr (generate-new-buffer " *magit-ci-err*"))
         (proc   (make-process
                  :name     "magit-ci"
                  :buffer   stdout
                  :stderr   stderr
                  :command  (cons bob/magit-ci-gh-program args)
                  :noquery  t
                  :connection-type 'pipe)))
    (set-process-sentinel proc (funcall sentinel stdout stderr))
    (puthash key proc bob/magit-ci--inflight)
    proc))

(defun bob/magit-ci--fetch (key)
  "Kick off async fetch for KEY (REPO-ROOT . SHA)."
  (let* ((repo-root (car key))
         (default-directory repo-root)
         (branch (magit-get-current-branch)))
    (cl-labels
        ((commit-fallback ()
           (let* ((sha (cdr key))
                  (url (format "repos/{owner}/{repo}/commits/%s/check-runs" sha))
                  (sentinel-fn
                   (lambda (out err)
                     (lambda (proc event)
                       (when (memq (process-status proc) '(exit signal))
                         (remhash key bob/magit-ci--inflight)
                         (let* ((exit (process-exit-status proc))
                                (out-s (with-current-buffer out (buffer-string)))
                                (err-s (with-current-buffer err (buffer-string)))
                                entry)
                           (ignore event)
                           (unwind-protect
                               (if (and (zerop exit) (> (length (string-trim out-s)) 0))
                                   (let ((checks (ignore-errors
                                                   (bob/magit-ci--parse-commit-checks out-s))))
                                     (setq entry
                                           (make-bob/magit-ci-entry
                                            :status 'ok
                                            :bucket (bob/magit-ci--aggregate-bucket checks)
                                            :checks checks
                                            :fetched-at (float-time)
                                            :message (and (null checks) "no checks for HEAD"))))
                                 (setq entry
                                       (make-bob/magit-ci-entry
                                        :status 'error
                                        :bucket 'none
                                        :checks nil
                                        :fetched-at (float-time)
                                        :message (string-trim
                                                  (if (> (length err-s) 0) err-s
                                                    (format "gh exited with %s" exit))))))
                             (kill-buffer out)
                             (kill-buffer err))
                           (when entry (bob/magit-ci--store key entry))
                           (bob/magit-ci--refresh-status-buffers repo-root)))))))
             (bob/magit-ci--start-process
              key repo-root
              (list "api" "-H" "Accept: application/vnd.github+json" url)
              sentinel-fn))))
      ;; Primary: gh pr checks for the current branch.
      (let ((sentinel-fn
             (lambda (out err)
               (bob/magit-ci--make-sentinel key repo-root out err #'commit-fallback))))
        (bob/magit-ci--start-process
         key repo-root
         (append (list "pr" "checks")
                 (when branch (list branch))
                 (list "--json" "name,state,bucket,link,workflow"))
         sentinel-fn)))))

(defun bob/magit-ci--maybe-refresh (key)
  "Start a fetch for KEY unless one is in flight or cache is fresh."
  (let ((entry (gethash key bob/magit-ci--cache)))
    (cond
     ((gethash key bob/magit-ci--inflight) nil)
     ((and entry
           (eq (bob/magit-ci-entry-status entry) 'ok)
           (< (- (float-time) (bob/magit-ci-entry-fetched-at entry))
              bob/magit-ci-cache-ttl))
      nil)
     (t (bob/magit-ci--fetch key)))))

;; ---------------------------------------------------------------------------
;; Section

(defun bob/magit-ci--summary (entry)
  "Return a propertized one-line summary string for ENTRY."
  (pcase (bob/magit-ci-entry-status entry)
    ('loading (propertize "loading…" 'font-lock-face 'bob/magit-ci-pending))
    ('error   (propertize (or (bob/magit-ci-entry-message entry) "error")
                          'font-lock-face 'bob/magit-ci-fail))
    ('ok
     (let* ((checks (bob/magit-ci-entry-checks entry))
            (bucket (bob/magit-ci-entry-bucket entry))
            (counts (let ((h (make-hash-table)))
                      (dolist (c checks)
                        (cl-incf (gethash (plist-get c :bucket) h 0)))
                      h))
            (parts  nil))
       (if (null checks)
           (propertize (or (bob/magit-ci-entry-message entry) "no checks")
                       'font-lock-face 'bob/magit-ci-skip)
         (dolist (b '(pass fail pending skip cancel))
           (let ((n (gethash b counts 0)))
             (when (> n 0)
               (push (propertize
                      (concat (bob/magit-ci--bucket-icon b)
                              " " (number-to-string n))
                      'font-lock-face (bob/magit-ci--bucket-face b))
                     parts))))
         (concat
          (propertize (concat (bob/magit-ci--bucket-icon bucket) " ")
                      'font-lock-face (bob/magit-ci--bucket-face bucket))
          (string-join (nreverse parts) "  ")))))
    (_ (propertize "unknown" 'font-lock-face 'bob/magit-ci-skip))))

;;;###autoload
(defun bob/magit-ci-insert-section ()
  "Insert a CI status section in `magit-status'."
  (when (and (executable-find bob/magit-ci-gh-program)
             (bob/magit-ci--github-remote-p))
    (let* ((key   (bob/magit-ci--key))
           (entry (and key (gethash key bob/magit-ci--cache))))
      (when key
        ;; Fire fetch (non-blocking) if needed.
        (bob/magit-ci--maybe-refresh key)
        (unless entry
          (setq entry (make-bob/magit-ci-entry
                       :status 'loading :bucket 'pending
                       :checks nil :fetched-at (float-time))))
        (magit-insert-section (bob/magit-ci nil t) ; collapsed by default
          (magit-insert-heading
            (concat (propertize (format "%-10s" "CI:")
                                'font-lock-face 'bob/magit-ci-heading)
                    (bob/magit-ci--summary entry)))
          (when (and (eq (bob/magit-ci-entry-status entry) 'ok)
                     (bob/magit-ci-entry-checks entry))
            (let* ((checks (bob/magit-ci-entry-checks entry))
                   (shown  (seq-take checks bob/magit-ci-max-checks-shown))
                   (extra  (- (length checks) (length shown))))
              (dolist (c shown)
                (let* ((bucket (plist-get c :bucket))
                       (face   (bob/magit-ci--bucket-face bucket))
                       (icon   (bob/magit-ci--bucket-icon bucket))
                       (name   (plist-get c :name))
                       (wf     (plist-get c :workflow)))
                  (magit-insert-section (bob/magit-ci-check c)
                    (insert "  "
                            (propertize icon 'font-lock-face face)
                            " "
                            (propertize name 'font-lock-face face)
                            (if (and wf (not (string-empty-p wf)))
                                (propertize (concat "  (" wf ")")
                                            'font-lock-face 'magit-dimmed)
                              "")
                            "\n"))))
              (when (> extra 0)
                (insert (propertize (format "  … %d more\n" extra)
                                    'font-lock-face 'magit-dimmed)))))
          (insert "\n"))))))

;;;###autoload
(defun bob/magit-ci-refresh ()
  "Force-refresh CI status for the current repository."
  (interactive)
  (when-let ((key (bob/magit-ci--key)))
    (remhash key bob/magit-ci--cache)
    (bob/magit-ci--maybe-refresh key)
    (message "magit-ci: refreshing…")))

;;;###autoload
(defun bob/magit-ci-visit-check ()
  "Open the link of the CI check at point in a browser.
When point is on a non-CI section, fall back to `magit-visit-thing'
so that e.g. forge topics open their normal link."
  (interactive)
  (let* ((sec (magit-current-section))
         (val (and sec
                   (eq (oref sec type) 'bob/magit-ci-check)
                   (oref sec value)))
         (link (and val (plist-get val :link))))
    (cond
     ((and link (not (string-empty-p link)))
      (browse-url link))
     (val
      (user-error "No link for this check"))
     ((fboundp 'forge-browse)
      (call-interactively #'forge-browse))
     (t
      (call-interactively #'magit-visit-thing)))))

(with-eval-after-load 'magit
  (define-key magit-mode-map (kbd "C-c C-i") #'bob/magit-ci-refresh)
  (define-key magit-mode-map (kbd "C-c C-o") #'bob/magit-ci-visit-check))

(provide 'magit-ci)
;;; magit-ci.el ends here
