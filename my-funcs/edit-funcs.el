;;; name.el --- summary -*- lexical-binding: t -*-

;; Author: adam bob
;; Maintainer: adam bob
;; Version: version
;; Package-Requires: (dependencies)
;; Homepage: homepage
;; Keywords: keywords


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; commentary

;;; Code:
(require 's)
(require 'dash)
(require 'cl)

(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated.
However, if there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg)))))

(defun my/set-shell-buffer-name ()
  "Set buffer name in e/shell modes."
  (interactive)
  (cond ((eq major-mode 'shell-mode)
         (rename-buffer (my/get-buffer-name default-directory "shell/")))
        ((eq major-mode 'eshell-mode)
         (rename-buffer (my/get-buffer-name default-directory "eshell/")))))

(defun my/get-buffer-name (path prefix)
  "Get last string from a file system PATH with PREFIX."
  (let ((s-str (remove-duplicates (s-split "/\\|@\\||\\|:" path t)
                                  :test (lambda (x y) (or (null y) (equal x y)))
                   :from-end t)))
    (my/gen-buffer-name-from-string-list s-str prefix)))

(defun my/gen-buffer-name-from-string-list (str-list new-name)
  "Get last non empty string from STR-LIST.
Concat meaningful names into the new buffer name recursivly.
Use NEW-NAME as the new name prefix."
  (let ((next-str (get-meanigfull-name (car str-list))))
    (if (cdr str-list)
        (my/gen-buffer-name-from-string-list (cdr str-list)
                                             (concat new-name next-str (and next-str "/")))
      new-name)))

(my/get-buffer-name "/ssh:ubuntu@prod-sam-11|sudo:root@prod-sam-11:/var/log/bigpanda/" "eshell/")

(defun get-meanigfull-name (meaningfull?)
  "Check if MEANINGFULL? is meaningfull and return it if so.
If not return \"\""
  (cond ((s-contains? "prod" meaningfull?) meaningfull?)
        ((s-contains? "perf" meaningfull?) meaningfull?)
        ((s-contains? "od-" meaningfull?) meaningfull?)
        ((s-contains? "log" meaningfull?) meaningfull?)))

(defun split-multi-sep (str &rest separators)
  "Split STR with SEPARATORS."
  (-flatten (if (car separators)
       (mapcar (lambda (s)
                 (apply 'split-multi-sep (cons s (cdr separators))))
               (split-string str (car separators)))
     str)))
(provide 'edit-funcs)
;;; edit-funcs.el ends here

;;; name.el ends here
