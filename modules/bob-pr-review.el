;;; bob-pr-review.el --- Emacs helpers for incremental PR review -*- lexical-binding: t; -*-
;; Loaded from the Emacs init so agent skills can call pr-review functions
;; directly. Complex narrated reviews use the solveit runtime loaded from the
;; Emacs config as well.

(defvar pr-review--overlays nil
  "Active review highlight overlays.")

(defvar pr-review--base-branch "develop"
  "Base branch for diff reference.")

(defun pr-review-focus-region (file start end)
  "Open FILE, highlight lines START to END with a subtle background, and scroll to them."
  (pr-review-clear)
  (let ((buf (find-file-noselect file)))
    (switch-to-buffer buf)
    (select-frame-set-input-focus (selected-frame))
    (goto-char (point-min))
    (forward-line (1- start))
    (let* ((beg (line-beginning-position))
           (_ (forward-line (- end start)))
           (fin (line-end-position))
           (ov (make-overlay beg fin)))
      (overlay-put ov 'face 'secondary-selection)
      (overlay-put ov 'pr-review t)
      (push ov pr-review--overlays)
      (goto-char beg)
      (recenter 3))))

(defun pr-review-show-hunk (file line)
  "Open FILE, go to LINE, and show the diff hunk inline via `diff-hl-show-hunk'."
  (let ((buf (find-file-noselect file)))
    (switch-to-buffer buf)
    (select-frame-set-input-focus (selected-frame))
    (when (fboundp 'diff-hl-set-reference-rev)
      (diff-hl-set-reference-rev pr-review--base-branch))
    (goto-char (point-min))
    (forward-line (1- line))
    (when (fboundp 'diff-hl-show-hunk)
      (diff-hl-show-hunk))))

(defun pr-review-show-file-diff (file)
  "Open FILE and show its full diff against the base branch via `vc-version-diff'."
  (let ((buf (find-file-noselect file)))
    (switch-to-buffer buf)
    (select-frame-set-input-focus (selected-frame))
    (vc-version-diff (list buffer-file-name) pr-review--base-branch nil)
    (delete-other-windows)))

(defun pr-review-set-base-branch (branch)
  "Set the base BRANCH for diffs."
  (setq pr-review--base-branch branch)
  (when (fboundp 'diff-hl-set-reference-rev)
    (diff-hl-set-reference-rev branch)))

(defun pr-review-clear ()
  "Remove all review highlight overlays."
  (dolist (ov pr-review--overlays)
    (when (overlay-buffer ov)
      (delete-overlay ov)))
  (setq pr-review--overlays nil))

(provide 'bob-pr-review)
