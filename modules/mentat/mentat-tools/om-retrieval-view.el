;;; om-retrieval-view.el --- Show OM reflection retrieval in Mentat -*- lexical-binding: t; -*-
;;; pi-load-after-edit: t

;;; Commentary:
;; Render the experimental OM `om.reflections.retrieved' custom message as a
;; collapsed card: how many reflections this prompt added and how many
;; retrieved reflections the model currently sees.  RET expands the list.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'vui)
(require 'mentat-buffer)

(defconst bob/om-retrieval-view-type "om.reflections.retrieved"
  "OM custom-message type for retrieved reflections.")

(defun bob/om-retrieval-view--details-p (details)
  "Return non-nil when DETAILS is a supported retrieval payload."
  (and (listp details)
       (memq (alist-get 'version details) '(1 2))
       (sequencep (alist-get 'selected details))
       (numberp (alist-get 'tokens details))
       (let ((in-context (alist-get 'inContext details)))
         (and (listp in-context)
              (numberp (alist-get 'count in-context))
              (numberp (alist-get 'tokens in-context))))))

(defun bob/om-retrieval-view--plural (count noun)
  "Return COUNT followed by NOUN, pluralized."
  (format "%d %s%s" count noun (if (= count 1) "" "s")))

(defun bob/om-retrieval-view-header (details)
  "Return the one-line card header for retrieval DETAILS."
  (let ((selected (length (alist-get 'selected details)))
        (in-context (alist-get 'inContext details)))
    (format "⟲ Retrieved %s · ~%d tokens · in context: %s, ~%d tokens"
            (bob/om-retrieval-view--plural selected "reflection")
            (alist-get 'tokens details)
            (bob/om-retrieval-view--plural (alist-get 'count in-context) "reflection")
            (alist-get 'tokens in-context))))

(defun bob/om-retrieval-view--scores (item details)
  "Format ITEM's scores using the question version in DETAILS."
  (if (= 2 (alist-get 'version details))
      (format "relevance %.2f, missing %.2f"
              (alist-get 'relevance item) (alist-get 'missing item))
    (format "relevance %.2f, value %.2f"
            (alist-get 'materialChange item) (alist-get 'highValue item))))

(defun bob/om-retrieval-view-body (text details)
  "Return Markdown listing the reflections in TEXT with scores from DETAILS."
  (let ((scores (make-hash-table :test #'equal)))
    (seq-doseq (item (alist-get 'selected details))
      (puthash (alist-get 'id item) item scores))
    (string-join
     (delq nil
           (mapcar
            (lambda (line)
              (when (string-match "\\`\\[\\([a-f0-9]\\{12\\}\\)\\] \\(.*\\)\\'" line)
                (let* ((id (match-string 1 line))
                       (item (gethash id scores)))
                  (format "- `%s` %s%s" id (match-string 2 line)
                          (if item
                              (format " *(%s)*" (bob/om-retrieval-view--scores item details))
                            "")))))
            (split-string (or text "") "\n")))
     "\n")))

(defun bob/om-retrieval-view--rejection (item details)
  "Explain which of ITEM's scores missed the cutoffs in DETAILS."
  (if (= 2 (alist-get 'version details))
      (let ((thresholds (alist-get 'thresholds details)))
        (string-join
         (delq nil
               (mapcar (lambda (key)
                         (let ((cutoff (alist-get key thresholds)))
                           (when (< (alist-get key item) cutoff)
                             (format "%s < %.2f" key cutoff))))
                       '(relevance missing)))
         ", "))
    (let ((threshold (alist-get 'threshold details)))
      (format "below %.2f: %s" threshold
              (string-join
               (delq nil (list (when (< (alist-get 'materialChange item) threshold) "relevance")
                               (when (< (alist-get 'highValue item) threshold) "value")))
               ", ")))))

(defun bob/om-retrieval-view-near-misses (details)
  "Return Markdown for UI-only rejected reflections in DETAILS."
  (string-join
   (mapcar
    (lambda (item)
      (format "- `%s` %s *(%s; %s)*"
              (alist-get 'id item) (alist-get 'content item)
              (bob/om-retrieval-view--scores item details)
              (bob/om-retrieval-view--rejection item details)))
    (append (alist-get 'nearMisses details) nil))
   "\n"))

(vui-defcomponent bob/om-retrieval-view
    (header body expanded on-toggle entry-id near-body near-expanded on-toggle-near near-count)
  "Render a retrieval HEADER that toggles BODY when EXPANDED."
  :render
  (let ((line (propertize (concat "\n" header "\n")
                          'face 'mentat-notification-info-face
                          'mentat-entry-id entry-id)))
    (vui-vstack
     (vui-text (mentat--buffer-action-button
                line "RET or mouse-1 shows or hides retrieved reflections"
                on-toggle t))
     (when expanded
       (vui-vstack
        (vui-text body)
        (when near-body
          (vui-vstack
           (vui-text (mentat--buffer-action-button
                      (format "\nNear misses (%d)\n" near-count)
                      "RET or mouse-1 shows or hides rejected reflections"
                      on-toggle-near))
           (when near-expanded (vui-text near-body)))))))))

(defun bob/om-retrieval-view--node
    (stream-node header body expanded entry-id near-body near-expanded near-count)
  "Build STREAM-NODE's card with HEADER, BODY and ENTRY-ID.
EXPANDED controls BODY.  NEAR-BODY, NEAR-EXPANDED and NEAR-COUNT describe
its independently expandable near misses."
  (vui-component 'bob/om-retrieval-view
    :header header :body body :expanded expanded :entry-id entry-id
    :near-body near-body :near-expanded near-expanded :near-count near-count
    :on-toggle
    (lambda ()
      (vui-stream-update
       stream-node
       (bob/om-retrieval-view--node
        stream-node header body (not expanded) entry-id near-body near-expanded near-count)))
    :on-toggle-near
    (lambda ()
      (vui-stream-update
       stream-node
       (bob/om-retrieval-view--node
        stream-node header body expanded entry-id near-body (not near-expanded) near-count)))))

(defun bob/om-retrieval-view--render (view text content details entry-id)
  "Render a retrieval message with TEXT, CONTENT, DETAILS and ENTRY-ID in VIEW."
  ;; Like other notices, keep the card outside the preceding response.
  (mentat--buffer-update-response-card view t)
  (let* ((header (bob/om-retrieval-view-header details))
         (body (mentat--buffer-pi-update-body
                (if (seq-empty-p (alist-get 'selected details))
                    "No reflections passed the cutoff."
                  (bob/om-retrieval-view-body text details))
                (mentat--buffer-markdown-context view)))
         (near-count (length (alist-get 'nearMisses details)))
         (near-body (when (> near-count 0)
                      (mentat--buffer-pi-update-body
                       (bob/om-retrieval-view-near-misses details)
                       (mentat--buffer-markdown-context view))))
         (node (vui-stream-open-row
                (mentat--buffer-stream view)
                (bob/om-retrieval-view--node nil header body nil entry-id near-body nil near-count))))
    (setf (mentat--buffer-entries view)
          (append (mentat--buffer-entries view)
                  (list (list :role 'custom :custom-type bob/om-retrieval-view-type
                              :text text :content content :details details
                              :entry-id entry-id))))
    (vui-stream-update
     node (bob/om-retrieval-view--node node header body nil entry-id near-body nil near-count))))

(defun bob/om-retrieval-view--around (orig view custom-type text content details entry-id)
  "Render retrieval messages as cards; pass others to ORIG.
VIEW, CUSTOM-TYPE, TEXT, CONTENT, DETAILS and ENTRY-ID are as for ORIG."
  (if (and (equal custom-type bob/om-retrieval-view-type)
           (bob/om-retrieval-view--details-p details))
      (bob/om-retrieval-view--render view text content details entry-id)
    (funcall orig view custom-type text content details entry-id)))

(advice-add 'mentat--buffer-render-custom-message :around #'bob/om-retrieval-view--around)

(provide 'om-retrieval-view)
;;; om-retrieval-view.el ends here
