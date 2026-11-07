;;; bob-prose-completion.el --- Ranked prose word completion -*- lexical-binding: t -*-

;;; Commentary:

;; Ranked prose word completion shared by ordinary buffers and the
;; private Mentat adapter.

;;; Code:

(require 'cl-lib)
(require 'project)
(require 'seq)
(require 'subr-x)

(defvar bob/prose-word-completion-frequency-file
  "~/.pi/agent/extensions/src/vendor/wordfreq-en-25000-log.tsv")

(defvar bob/prose-word-completion-project-buffer-limit 24)
(defvar bob/prose-word-completion-project-buffer-max-chars 60000)
(defvar bob/prose-word-completion-prefix-index-length 2)
(defvar bob/prose-word-completion-min-prefix 2)
(defvar bob/prose-word-completion-candidate-limit 200)
(defvar bob/mentat-prose-word-project-score 80)
(defvar bob/mentat-prose-word-known-project-bonus 5)

(defvar bob/prose-word-completion--cache nil)
(defvar bob/prose-word-completion--cache-file nil)
(defvar-local bob/prose-word-completion--buffer-word-cache nil)
(defvar-local bob/prose-word-completion--project-membership-cache nil)
(defvar-local bob/prose-word-completion--symbol-count-cache nil)

(defun bob/prose-word-completion--bucket-key (word)
  (let ((word (downcase word)))
    (when (>= (length word) bob/prose-word-completion-prefix-index-length)
      (substring word 0 bob/prose-word-completion-prefix-index-length))))

(defun bob/prose-word-completion--build-word-buckets (words)
  (let ((buckets (make-hash-table :test #'equal)))
    (dolist (word words buckets)
      (when-let* ((key (bob/prose-word-completion--bucket-key word)))
        (push word (gethash key buckets))))))

(defun bob/prose-word-completion--bucket-words (buckets prefix)
  (when-let* ((key (bob/prose-word-completion--bucket-key prefix)))
    (gethash key buckets)))

(defun bob/prose-word-completion--build-cache (file)
  (with-temp-buffer
    (insert-file-contents file)
    (let ((frequencies (make-hash-table :test #'equal)) words
          (file-lines (split-string (buffer-string) "\n" t)))
      (dolist (line file-lines)
        (pcase-let ((`(,word ,frequency . ,_)
                     (split-string line "\t")))
          (when (and word frequency
                     (string-match-p "\\`[a-z][a-z'-]+\\'" word))
            (unless (gethash word frequencies)
              (push word words))
            (puthash word (string-to-number frequency) frequencies))))
      (setq words (sort words #'string<))
      (list :words words
            :frequencies frequencies
            :buckets (bob/prose-word-completion--build-word-buckets words)))))

(defun bob/prose-word-completion--frequency-data ()
  "Return cached word-frequency data as (WORDS . TABLE)."
  (when-let* ((file (expand-file-name bob/prose-word-completion-frequency-file))
              (file-readable-p file))
    (if (and (equal file bob/prose-word-completion--cache-file)
             bob/prose-word-completion--cache)
        bob/prose-word-completion--cache
      (setq bob/prose-word-completion--cache-file file
            bob/prose-word-completion--cache
            (bob/prose-word-completion--build-cache file)))))

(defun bob/prose-word-completion--bounds ()
  (let ((end (point)) start)
    (when (not (looking-at-p "[[:alpha:]'-]"))
      (save-excursion
        (skip-chars-backward "[:alpha:]'-")
        (setq start (point)))
      (when (and start
                 (>= (- end start) bob/prose-word-completion-min-prefix)
                 (save-excursion (goto-char start) (looking-at-p "[[:alpha:]]"))
                 (not (memq (char-before start) '(?/ ?@ ?# ?$ ?. ?:))))
        (cons start end)))))

(defun bob/prose-word-completion--frequency-score (word)
  (let* ((data (bob/prose-word-completion--frequency-data))
         (frequencies (plist-get data :frequencies))
         (frequency (and frequencies (gethash (downcase word) frequencies))))
    (if frequency
        (* 30 (max 0 (min 1 (/ (- frequency -13.7)
                                (- -2.8 -13.7)))))
      0)))

(defun bob/prose-word-completion--score (word prefix source-score)
  (+ source-score
     (bob/prose-word-completion--frequency-score word)
     (- (- (length word) (length prefix)))))

(defun bob/prose-word-completion--ordinary-titlecase-p (word)
  "Return non-nil when WORD has narrow ordinary titlecase."
  (let ((case-fold-search nil))
    (string-match-p "\\`[[:upper:]][[:lower:]][[:lower:]'-]+\\'" word)))

(defun bob/prose-word-completion--add-candidate (table word prefix score)
  (let ((normalized (downcase word)))
    (when (and (not (equal normalized (downcase prefix)))
               (string-prefix-p prefix word t)
               (string-match-p "\\`[[:alpha:]][[:alpha:]'-]\\{2,\\}\\'" word))
      (let* ((existing (gethash normalized table))
             (existing-word (and existing (car existing)))
             (canonical
              (cond
               ((bob/prose-word-completion--ordinary-titlecase-p word)
                word)
               ((and existing-word
                     (bob/prose-word-completion--ordinary-titlecase-p
                      existing-word))
                existing-word)
               ((or (not existing) (> score (cdr existing))) word)
               (t existing-word))))
        (puthash normalized
                 (cons canonical
                       (if existing
                           (max score (cdr existing))
                         score))
                 table)))))

(defun bob/prose-word-completion--case-group (word prefix)
  "Return WORD's case variants, preferring the case implied by PREFIX.
A capitalized PREFIX also offers a lowercase WORD capitalized, as at the
start of a sentence."
  (let ((capitalized-prefix (let ((case-fold-search nil))
                              (string-match-p "\\`[[:upper:]]" prefix))))
    (cond
     ((bob/prose-word-completion--ordinary-titlecase-p word)
      (if capitalized-prefix
          (list word (downcase word))
        (list (downcase word) word)))
     ((and capitalized-prefix (equal word (downcase word)))
      (list (concat (upcase (substring word 0 1)) (substring word 1))
            word))
     (t (list word)))))

(defun bob/prose-word-completion--words-in-buffer (&optional max-chars)
  "Return cached prose words from current buffer, scanning at most MAX-CHARS."
  (let* ((tick (buffer-chars-modified-tick))
         (limit max-chars)
         (cache bob/prose-word-completion--buffer-word-cache))
    (if (and cache
             (equal (nth 0 cache) tick)
             (equal (nth 1 cache) limit))
        (nth 2 cache)
      (let ((words nil)
            (seen (make-hash-table :test #'equal))
            (case-fold-search nil))
        (save-excursion
          (save-restriction
            (widen)
            (goto-char (point-min))
            (let ((end (if limit
                           (min (point-max) (+ (point-min) limit))
                         (point-max))))
              (while (re-search-forward "[[:alpha:]][[:alpha:]'-]\\{2,\\}" end t)
                (let* ((word (match-string-no-properties 0))
                       (normalized (downcase word))
                       (existing (gethash normalized seen)))
                  (cond
                   ((null existing)
                    (puthash normalized word seen)
                    (push word words))
                   ((and (bob/prose-word-completion--ordinary-titlecase-p word)
                         (not (bob/prose-word-completion--ordinary-titlecase-p
                               existing)))
                    (setcar (member existing words) word)
                    (puthash normalized word seen))))))))
        (setq words (nreverse words))
        (setq bob/prose-word-completion--buffer-word-cache
              (list tick limit words
                    (bob/prose-word-completion--build-word-buckets words)))
        (nth 2 bob/prose-word-completion--buffer-word-cache)))))

(defun bob/prose-word-completion--bucket-words-in-buffer (prefix &optional max-chars)
  "Return cached prose words from current buffer's PREFIX bucket."
  (bob/prose-word-completion--words-in-buffer max-chars)
  (bob/prose-word-completion--bucket-words
   (nth 3 bob/prose-word-completion--buffer-word-cache) prefix))

(defun bob/prose-word-completion--symbol-counts ()
  "Return a cached table of symbol occurrence counts in the current buffer."
  (let ((tick (buffer-chars-modified-tick)))
    (unless (equal (car bob/prose-word-completion--symbol-count-cache) tick)
      (let ((counts (make-hash-table :test #'equal)))
        (save-excursion
          (save-restriction
            (widen)
            (goto-char (point-min))
            (while (re-search-forward "\\(?:\\sw\\|\\s_\\)+" nil t)
              (let ((symbol (match-string-no-properties 0)))
                (puthash symbol (1+ (gethash symbol counts 0)) counts)))))
        (setq bob/prose-word-completion--symbol-count-cache (cons tick counts))))
    (cdr bob/prose-word-completion--symbol-count-cache)))

(defun bob/prose-word-completion--eglot-context-sort (completions)
  "Sort Eglot COMPLETIONS by server rank, then by count in the buffer."
  (let ((counts (bob/prose-word-completion--symbol-counts)))
    (cl-flet ((rank (completion)
                (or (plist-get (get-text-property 0 'eglot--lsp-item completion)
                               :sortText)
                    "")))
      (sort completions
            (lambda (a b)
              (let ((rank-a (rank a))
                    (rank-b (rank b)))
                (or (string< rank-a rank-b)
                    (and (string= rank-a rank-b)
                         (> (gethash (substring-no-properties a) counts 0)
                            (gethash (substring-no-properties b) counts 0))))))))))

(defun bob/prose-word-completion-add-eglot-context-sort (result)
  "Add buffer-context ordering to Eglot completion RESULT.
Only the inline preview and cycling read `cycle-sort-function'; Corfu's
popup keeps Eglot's `display-sort-function'."
  (and result
       (append result
               (list :cycle-sort-function
                     #'bob/prose-word-completion--eglot-context-sort))))

(defun bob/prose-word-completion--add-words (table words prefix score)
  (dolist (word words)
    (bob/prose-word-completion--add-candidate
     table word prefix
     (bob/prose-word-completion--score word prefix score))))

(defun bob/prose-word-completion--add-buffer (table buffer prefix score
                                                    &optional max-chars)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (bob/prose-word-completion--add-words
       table (bob/prose-word-completion--bucket-words-in-buffer prefix max-chars)
       prefix score))))

(defun bob/prose-word-completion--add-text (table text prefix score)
  (with-temp-buffer
    (insert text)
    (bob/prose-word-completion--add-buffer table (current-buffer) prefix score)))

(defun bob/prose-word-completion--project-root ()
  (when-let* ((project (project-current nil)))
    (expand-file-name (project-root project))))

(defun bob/prose-word-completion--project-file-buffers (&optional project-root)
  "Return open file buffers under PROJECT-ROOT or the current project."
  (when-let* ((root (or project-root
                        (bob/prose-word-completion--project-root))))
    (let ((buffers nil)
          (count 0))
      (dolist (buffer (buffer-list) (nreverse buffers))
        (when (and (< count bob/prose-word-completion-project-buffer-limit)
                   (not (eq buffer (current-buffer))))
          (with-current-buffer buffer
            (when-let* ((file buffer-file-name))
              (unless (file-remote-p file)
                (let* ((key (list root file))
                       (cache bob/prose-word-completion--project-membership-cache))
                  (unless (equal key (car cache))
                    (setq cache
                          (cons key (file-in-directory-p
                                     (expand-file-name file) root)))
                    (setq bob/prose-word-completion--project-membership-cache
                          cache))
                  (when (cdr cache)
                    (setq count (1+ count))
                    (push buffer buffers)))))))))))

(defun bob/prose-word-completion--add-project-buffers (table prefix score)
  (dolist (buffer (bob/prose-word-completion--project-file-buffers))
    (bob/prose-word-completion--add-buffer
     table buffer prefix score
     bob/prose-word-completion-project-buffer-max-chars)))

(defun bob/mentat-prose-word-candidates (prefix context)
  "Add frequency and project candidates for Mentat PREFIX and CONTEXT."
  (let* ((project-words (make-hash-table :test #'equal))
         (data (bob/prose-word-completion--frequency-data))
         (frequencies (and data (plist-get data :frequencies)))
         candidates)
    (dolist (buffer
             (bob/prose-word-completion--project-file-buffers
              (plist-get context :project-root)))
      (with-current-buffer buffer
        (dolist (word
                 (bob/prose-word-completion--bucket-words-in-buffer
                  prefix bob/prose-word-completion-project-buffer-max-chars))
          (let* ((normalized (downcase word))
                 (existing (gethash normalized project-words)))
            (when (or (null existing)
                      (bob/prose-word-completion--ordinary-titlecase-p word)
                      (not (bob/prose-word-completion--ordinary-titlecase-p
                            existing)))
              (puthash normalized word project-words))))))
    (maphash
     (lambda (normalized word)
       (push (list :word word
                   :score (if (and frequencies
                                   (gethash normalized frequencies))
                              bob/mentat-prose-word-known-project-bonus
                            bob/mentat-prose-word-project-score)
                   :source 'project
                   :annotation " project")
             candidates))
     project-words)
    (when data
      (let ((added 0))
        (catch 'done
          (dolist (word
                   (bob/prose-word-completion--bucket-words
                    (plist-get data :buckets) prefix))
            (when (string-prefix-p prefix word t)
              (push (list :word word
                          :score (+ 60
                                    (bob/prose-word-completion--frequency-score
                                     word))
                          :source 'frequency
                          :annotation " word")
                    candidates)
              (setq added (1+ added))
              (when (>= added 200)
                (throw 'done nil))))))
      (when (and (not (string-suffix-p "s" prefix t))
                 (gethash (downcase prefix) frequencies))
        (push (list :word (concat prefix "s") :score 40 :source 'plural
                    :annotation " plural")
              candidates)))
    candidates))

(defun bob/mentat-prose-word-candidate-score (candidate prefix)
  "Score merged Mentat CANDIDATE for PREFIX with a mild length cost."
  (- (plist-get candidate :score)
     (max 0 (- (length (plist-get candidate :word))
               (length prefix)))))

(defun bob/prose-word-completion-at-point ()
  "Ranked prose word CAPF for natural-language buffers."
  (when-let* ((bounds (bob/prose-word-completion--bounds))
              (prefix (buffer-substring-no-properties (car bounds) (cdr bounds))))
    (let ((table (make-hash-table :test #'equal)))
      (bob/prose-word-completion--add-buffer table (current-buffer) prefix 300)
      (bob/prose-word-completion--add-project-buffers table prefix 180)
      (when-let* ((data (bob/prose-word-completion--frequency-data)))
        (let ((added 0))
          (catch 'done
            (dolist (word (bob/prose-word-completion--bucket-words
                           (plist-get data :buckets) prefix))
              (when (string-prefix-p prefix word t)
                (setq added (1+ added))
                (bob/prose-word-completion--add-candidate
                 table word prefix
                 (bob/prose-word-completion--score word prefix 60))
                (when (>= added 200)
                  (throw 'done nil)))))))
      (when-let* ((data (bob/prose-word-completion--frequency-data)))
        (when (and (not (string-suffix-p "s" prefix t))
                   (gethash (downcase prefix) (plist-get data :frequencies)))
          (bob/prose-word-completion--add-candidate table (concat prefix "s")
                                                    prefix 40)))
      (let ((semantic-candidates
             (sort (hash-table-values table)
                   (lambda (a b)
                     (let ((score-diff (- (cdr a) (cdr b))))
                       (if (/= score-diff 0)
                           (> score-diff 0)
                         (string< (car a) (car b)))))))
            candidates)
        (dolist (word semantic-candidates)
          (let ((group (bob/prose-word-completion--case-group (car word) prefix)))
            (when (<= (+ (length candidates) (length group))
                      (max 0 bob/prose-word-completion-candidate-limit))
              (setq candidates (nconc candidates group)))))
        (when candidates
          (list (car bounds) (cdr bounds)
                candidates
                :exclusive 'no
                :annotation-function (lambda (_) " word")
                ;; Keep the ranking above in previews and shown completions.
                :display-sort-function #'identity
                :cycle-sort-function #'identity))))))

(defun bob/prose-word-completion-comment-at-point ()
  "Ranked prose word CAPF inside comments and strings."
  (when (nth 8 (syntax-ppss))
    (bob/prose-word-completion-at-point)))

(defun bob/prose-word-completion-setup ()
  "Use Bob's ranked prose CAPF in natural-language buffers."
  (add-hook 'completion-at-point-functions
            #'bob/prose-word-completion-at-point nil t))

(defun bob/prose-word-completion-prog-setup ()
  "Offer prose words in comments and strings before code completion.
The prose CAPF is non-exclusive, so code CAPFs such as Eglot's still
run when it has no match."
  (add-hook 'completion-at-point-functions
            #'bob/prose-word-completion-comment-at-point -90 t))

(provide 'bob-prose-completion)
;;; bob-prose-completion.el ends here
