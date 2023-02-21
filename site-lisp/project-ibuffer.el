;;; project-ibuffer.el --- project.el ibuffer functions -*- lexical-binding: t -*-

;; Author: Adam Bobrow
;; Maintainer: Adam Bobrow
;; Version: 0.1
;; Package-Requires: (ibuffer)
;; Homepage: homepage
;; Keywords: keywords


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; Adds project.el ibuffer support
;; This code is mostly copied from projectile.el

;;; Code:


(defun project--name (&optional project)
 (let  ((pr (or project (project-root (project-current))))) 
  (file-name-nondirectory (directory-file-name pr))))

(define-ibuffer-filter project-files
    "Shows the current project's active file buffers"
  (:reader (read-directory-name "Project root: " (project-root (project-current))))
  (with-current-buffer buf
    (let ((directory (file-name-as-directory (expand-file-name qualifier))))
      (project-buffer-p buf directory))))

(defun project-buffer-p (buffer project-root)
  "Check if BUFFER is under PROJECT-ROOT."
  (with-current-buffer buffer
    (let ((directory (if buffer-file-name
                         (file-name-directory buffer-file-name)
                       default-directory)))
      (and (not (string-prefix-p " " (buffer-name buffer)))
           directory
           (string-equal (file-remote-p directory)
                         (file-remote-p project-root))
           (not (string-match-p "^http\\(s\\)?://" directory))
           (string-prefix-p project-root (file-truename directory) (eq system-type 'windows-nt))))))

(defun project-ibuffer ()
  (interactive)
  "Open an IBuffer window showing all buffers in PROJECT-ROOT."
  (let* ((pr-current (project-current))
         (pr-root (and pr-current (project-root pr-current))))
   (if pr-root
       (ibuffer nil (format "*%s Buffers*" (project--name))
                (list (cons 'project-files pr-root)))
     (message "project-ibuffer: Not in known project"))))

(provide 'project-ibuffer)

;;; project-ibuffer.el ends here
