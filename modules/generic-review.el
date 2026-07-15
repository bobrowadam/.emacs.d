;;; generic-review.el --- Capture annotated source links for review -*- lexical-binding: t; -*-
;;; pi-load-after-edit: t
;; Copyright (C) 2026

;; Author: Bobrow Adam
;; Keywords: tools, outlines
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:
;;
;; `generic-review-mode' collects point or region annotations in a transient,
;; project-scoped Org buffer.  That Org buffer is the only session state: save
;; it normally to persist it, or kill it to discard it.

;;; Code:

(require 'org)
(require 'org-capture)
(require 'ol)
(require 'project)
(require 'seq)
(require 'url-util)

(defgroup generic-review nil
  "Capture annotated source links in an Org review buffer."
  :group 'tools)

(defcustom generic-review-max-excerpt-length 4000
  "Maximum number of characters to retain in a captured source excerpt.

A nil value retains the entire point or region selection."
  :type '(choice (const :tag "No limit" nil) integer)
  :group 'generic-review)

(defvar-local generic-review--session-buffer nil
  "Review buffer associated with the current source buffer.")

(defvar-local generic-review--session-root nil
  "Project root represented by the current review buffer.")

(defvar generic-review--capture-context nil
  "Dynamically bound source metadata for the active Org capture.")

(defconst generic-review--capture-key "R"
  "Temporary Org capture template key used by `generic-review-add-comment'.")

(defconst generic-review--capture-template
  '("R" "Review comment" entry
    (function generic-review--capture-target)
    "* %?\n:PROPERTIES:\n:SOURCE: %(generic-review--capture-property :source)\n:BEGIN: %(generic-review--capture-property :begin)\n:END: %(generic-review--capture-property :end)\n:CREATED: %U\n:END:\n\n%(generic-review--capture-link)\n\n#+begin_src %(generic-review--capture-property :language)\n%(generic-review--capture-property :excerpt)\n#+end_src\n")
  "Org capture template used for a generic review comment.")

(defun generic-review--project-root ()
  "Return the current buffer's project root or `default-directory'."
  (file-name-as-directory
   (expand-file-name
    (if-let* ((project (project-current nil)))
        (project-root project)
      default-directory))))

(defun generic-review--project-name (root)
  "Return a display name for project ROOT."
  (file-name-nondirectory (directory-file-name root)))

(defun generic-review--find-session (root)
  "Return the live review buffer for ROOT, if any."
  (seq-find
   (lambda (buffer)
     (with-current-buffer buffer
       (equal generic-review--session-root root)))
   (buffer-list)))

(defun generic-review--create-session (root)
  "Create and return a new Org review buffer for project ROOT."
  (let ((buffer (generate-new-buffer
                 (format "*Review: %s*" (generic-review--project-name root)))))
    (with-current-buffer buffer
      (setq-local default-directory root)
      (org-mode)
      (setq-local generic-review--session-root root)
      (insert (format "#+title: Review — %s\n\n"
                      (generic-review--project-name root))))
    buffer))

(defun generic-review--session-buffer (&optional create)
  "Return the review buffer for the current project.

When CREATE is non-nil, create a new session if none is live."
  (let* ((root (generic-review--project-root))
         (local-session generic-review--session-buffer)
         (buffer (and (buffer-live-p local-session)
                      (with-current-buffer local-session
                        (and (equal generic-review--session-root root)
                             local-session))))
         (buffer (or buffer (generic-review--find-session root))))
    (when (and (not buffer) create)
      (setq buffer (generic-review--create-session root)))
    (when buffer
      (setq-local generic-review--session-buffer buffer))
    buffer))

(defun generic-review--source-bounds ()
  "Return the point or region bounds to capture from the current buffer."
  (if (use-region-p)
      (cons (region-beginning) (region-end))
    (cons (line-beginning-position) (line-end-position))))

(defun generic-review--source-link (start)
  "Return an Org link to the current source location at START."
  (or (save-excursion
        (goto-char start)
        (org-store-link nil nil))
      (format "generic-review-buffer:%s"
              (url-hexify-string (buffer-name)))))

(defun generic-review--excerpt (start end)
  "Return the source text from START through END, capped when configured."
  (let* ((excerpt (buffer-substring-no-properties start end))
         (limit generic-review-max-excerpt-length))
    (when (and limit (> (length excerpt) limit))
      (setq excerpt (concat (substring excerpt 0 limit)
                            "\n… [excerpt truncated]")))
    ;; Avoid prematurely closing the enclosing source block when reviewing Org.
    (let ((case-fold-search t))
      (replace-regexp-in-string "^#\\+end_src\\b" ",#+end_src" excerpt))))

(defun generic-review--source-context ()
  "Collect metadata for a comment at point or over the active region."
  (pcase-let* ((`(,start . ,end) (generic-review--source-bounds))
               (`(,begin-line . ,end-line)
                (cons (line-number-at-pos start)
                      (line-number-at-pos end))))
    (list :source (or buffer-file-name (buffer-name))
          :begin begin-line
          :end end-line
          :link (generic-review--source-link start)
          :language (string-remove-suffix "-mode" (symbol-name major-mode))
          :excerpt (generic-review--excerpt start end))))

(defun generic-review--capture-property (property)
  "Return PROPERTY from the active generic-review capture context."
  (or (plist-get generic-review--capture-context property) ""))

(defun generic-review--capture-link ()
  "Return a formatted Org link for the active generic-review capture."
  (let ((link (generic-review--capture-property :link))
        (description
         (format "%s:%s–%s"
                 (generic-review--capture-property :source)
                 (generic-review--capture-property :begin)
                 (generic-review--capture-property :end))))
    (with-temp-buffer
      (org-mode)
      (org-insert-link nil link description)
      (buffer-string))))

(defun generic-review--capture-target ()
  "Move point to the end of the active generic-review session buffer."
  (let ((buffer (plist-get generic-review--capture-context :session-buffer)))
    (unless (buffer-live-p buffer)
      (user-error "The generic review session no longer exists"))
    (set-buffer buffer)
    (widen)
    (goto-char (point-max))
    (unless (bolp)
      (insert "\n"))))

(defun generic-review--follow-buffer-link (path _)
  "Visit the live buffer encoded in generic-review link PATH."
  (if-let* ((buffer (get-buffer (url-unhex-string path))))
      (pop-to-buffer buffer)
    (user-error "Review source buffer no longer exists")))

(org-link-set-parameters "generic-review-buffer"
                         :follow #'generic-review--follow-buffer-link)

;;;###autoload
(defun generic-review-start ()
  "Enable review capture and select the current project's Org session."
  (interactive)
  (generic-review-mode 1)
  (pop-to-buffer (generic-review--session-buffer t)))

;;;###autoload
(defun generic-review-add-comment ()
  "Capture an Org comment for point or the active region.

The comment is stored in the current project's review buffer.  It includes a
link back to the source location and a bounded snapshot of the selected text."
  (interactive)
  (let ((generic-review--capture-context
         (append (list :session-buffer (generic-review--session-buffer t))
                 (generic-review--source-context)))
        (org-capture-templates
         (cons generic-review--capture-template org-capture-templates)))
    (org-capture nil generic-review--capture-key)))

;;;###autoload
(defun generic-review-open ()
  "Display the current project's live review session."
  (interactive)
  (if-let* ((buffer (generic-review--session-buffer)))
      (pop-to-buffer buffer)
    (user-error "No live review session; use `generic-review-start' first")))

(defvar-keymap generic-review-mode-map
  "C-c v s" #'generic-review-start
  "C-c v c" #'generic-review-add-comment
  "C-c v o" #'generic-review-open)

;;;###autoload
(define-minor-mode generic-review-mode
  "Minor mode for collecting annotated source links in an Org review buffer."
  :lighter " Review"
  :keymap generic-review-mode-map)

(provide 'generic-review)
;;; generic-review.el ends here
