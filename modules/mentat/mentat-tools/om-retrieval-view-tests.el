;;; om-retrieval-view-tests.el --- Retrieval card tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Live and replayed retrieval messages render the same collapsed card.

;;; Code:

(require 'ert)
(require 'mentat)
(load (expand-file-name "om-retrieval-view.el"
                        (file-name-directory (or load-file-name buffer-file-name))) nil t t)

(defconst bob/om-retrieval-view-test--details
  '((version . 1) (model . "jev-1.13.0") (threshold . 0.6) (candidateCount . 40)
    (selected . [((id . "aaaaaaaaaaaa") (score . 0.7) (materialChange . 0.7) (highValue . 0.9))])
    (nearMisses . [((id . "bbbbbbbbbbbb") (content . "Rejected private diagnostic.")
                    (score . 0.59) (materialChange . 0.8) (highValue . 0.59))])
    (tokens . 31) (inContext . ((count . 3) (tokens . 95)))))

(defconst bob/om-retrieval-view-test--details-v2
  '((version . 2) (model . "jev-1.13.0")
    (thresholds . ((relevance . 0.55) (missing . 0.50))) (candidateCount . 40)
    (selected . [((id . "aaaaaaaaaaaa") (score . 0.5) (relevance . 0.55) (missing . 0.5))])
    (nearMisses . [((id . "bbbbbbbbbbbb") (content . "Rejected for relevance.")
                    (score . 0.54) (relevance . 0.54) (missing . 0.9))
                   ((id . "cccccccccccc") (content . "Rejected for missing.")
                    (score . 0.49) (relevance . 0.9) (missing . 0.49))])
    (tokens . 31) (inContext . ((count . 3) (tokens . 95)))))

(defconst bob/om-retrieval-view-test--text
  "Retrieved reflections from earlier in this session:\n[aaaaaaaaaaaa] Never patch Claude Bridge for OM.")

(defmacro bob/om-retrieval-view-test--with-view (binding &rest body)
  "Bind BINDING to a temporary Mentat view while evaluating BODY."
  (declare (indent 1))
  `(let* ((vui-render-delay nil)
          (,binding (mentat--buffer-create (generate-new-buffer-name "*om-retrieval-test*")))
          (buffer (mentat--buffer-buffer ,binding)))
     (unwind-protect (progn ,@body)
       (when (buffer-live-p buffer) (kill-buffer buffer)))))

(defun bob/om-retrieval-view-test--text (view)
  "Return VIEW's transcript text."
  (with-current-buffer (mentat--buffer-buffer view)
    (buffer-substring-no-properties (point-min) (point-max))))

(ert-deftest bob/om-retrieval-view-renders-collapsed-card-and-toggles ()
  (bob/om-retrieval-view-test--with-view view
    (mentat--buffer-handle-event
     view `((type . "message_end") (entryId . "ret-1")
            (message (role . "custom") (customType . "om.reflections.retrieved")
                     (content . ,bob/om-retrieval-view-test--text) (display . t)
                     (details . ,bob/om-retrieval-view-test--details))))
    (let ((text (bob/om-retrieval-view-test--text view)))
      (should (string-match-p "Retrieved 1 reflection · ~31 tokens · in context: 3 reflections, ~95 tokens" text))
      (should-not (string-match-p "Never patch" text))
      (should-not (string-match-p "\\[om\\.reflections\\.retrieved\\]" text)))
    (with-current-buffer (mentat--buffer-buffer view)
      (goto-char (point-min))
      (search-forward "Retrieved 1 reflection")
      (button-activate (button-at (1- (point)))))
    (let ((text (bob/om-retrieval-view-test--text view)))
      (should (string-match-p "aaaaaaaaaaaa" text))
      (should (string-match-p "Never patch Claude Bridge for OM\\." text))
      (should (string-match-p "relevance 0\\.70, value 0\\.90" text)))
    (should (equal "om.reflections.retrieved"
                   (plist-get (car (mentat--buffer-entries view)) :custom-type)))))

(ert-deftest bob/om-retrieval-view-replay-matches-live ()
  (dolist (details (list bob/om-retrieval-view-test--details
                        bob/om-retrieval-view-test--details-v2))
    (let (live replay)
      (bob/om-retrieval-view-test--with-view view
        (mentat--buffer-handle-event
         view `((type . "message_end") (entryId . "ret-1")
                (message (role . "custom") (customType . "om.reflections.retrieved")
                         (content . ,bob/om-retrieval-view-test--text) (display . t)
                         (details . ,details))))
        (setq live (bob/om-retrieval-view-test--text view)))
      (bob/om-retrieval-view-test--with-view view
        (mentat--buffer-handle-entries
         view `((success . t) (command . "get_entries")
                (data (entries . [((type . "custom_message") (id . "ret-1") (parentId . nil)
                                   (customType . "om.reflections.retrieved")
                                   (content . ,bob/om-retrieval-view-test--text) (display . t)
                                   (details . ,details))])
                      (leafId . "ret-1"))))
        (setq replay (bob/om-retrieval-view-test--text view)))
      (should (equal live replay)))))

(ert-deftest bob/om-retrieval-view-shows-independent-cutoffs-and-new-labels ()
  (bob/om-retrieval-view-test--with-view view
    (mentat--buffer-handle-event
     view `((type . "message_end") (entryId . "ret-v2")
            (message (role . "custom") (customType . "om.reflections.retrieved")
                     (content . ,bob/om-retrieval-view-test--text) (display . t)
                     (details . ,bob/om-retrieval-view-test--details-v2))))
    (with-current-buffer (mentat--buffer-buffer view)
      (goto-char (point-min))
      (search-forward "Retrieved 1 reflection")
      (button-activate (button-at (1- (point)))))
    (should (string-match-p (regexp-quote "relevance 0.55, missing 0.50")
                            (bob/om-retrieval-view-test--text view)))
    (with-current-buffer (mentat--buffer-buffer view)
      (goto-char (point-min))
      (search-forward "Near misses (2)")
      (button-activate (button-at (1- (point)))))
    (let ((text (bob/om-retrieval-view-test--text view)))
      (should (string-match-p (regexp-quote "relevance 0.54, missing 0.90; relevance < 0.55") text))
      (should (string-match-p (regexp-quote "relevance 0.90, missing 0.49; missing < 0.50") text))
      (should-not (string-match-p "value" text)))))

(ert-deftest bob/om-retrieval-view-zero-selection-has-independent-near-miss-disclosure ()
  (bob/om-retrieval-view-test--with-view view
    (let ((details (copy-tree bob/om-retrieval-view-test--details)))
      (setf (alist-get 'selected details) []
            (alist-get 'tokens details) 0
            (alist-get 'inContext details) '((count . 0) (tokens . 0)))
      (mentat--buffer-handle-event
       view `((type . "message_end") (entryId . "ret-zero")
              (message (role . "custom") (customType . "om.reflections.retrieved")
                       (content . "") (display . t) (details . ,details)))))
    (should (string-match-p "Retrieved 0 reflections" (bob/om-retrieval-view-test--text view)))
    (should-not (string-match-p "Near misses" (bob/om-retrieval-view-test--text view)))
    (with-current-buffer (mentat--buffer-buffer view)
      (goto-char (point-min))
      (search-forward "Retrieved 0 reflections")
      (button-activate (button-at (1- (point)))))
    (let ((text (bob/om-retrieval-view-test--text view)))
      (should (string-match-p "No reflections passed" text))
      (should (string-match-p "Near misses (1)" text))
      (should-not (string-match-p "Rejected private diagnostic" text)))
    (with-current-buffer (mentat--buffer-buffer view)
      (goto-char (point-min))
      (search-forward "Near misses (1)")
      (button-activate (button-at (1- (point)))))
    (let ((text (bob/om-retrieval-view-test--text view)))
      (should (string-match-p "Rejected private diagnostic" text))
      (should (string-match-p
               (regexp-quote "relevance 0.80, value 0.59; below 0.60: value") text)))
    (with-current-buffer (mentat--buffer-buffer view)
      (goto-char (point-min))
      (search-forward "Near misses (1)")
      (button-activate (button-at (1- (point)))))
    (should-not (string-match-p "Rejected private diagnostic" (bob/om-retrieval-view-test--text view)))))

(ert-deftest bob/om-retrieval-view-leaves-other-and-malformed-messages-generic ()
  (bob/om-retrieval-view-test--with-view view
    (mentat--buffer-handle-event
     view '((type . "message_end") (entryId . "x")
            (message (role . "custom") (customType . "om.reflections.retrieved")
                     (content . "unexpected") (display . t) (details (version . 2)))))
    (should (string-match-p "\\[om\\.reflections\\.retrieved\\]" (bob/om-retrieval-view-test--text view)))))

(provide 'om-retrieval-view-tests)
;;; om-retrieval-view-tests.el ends here
