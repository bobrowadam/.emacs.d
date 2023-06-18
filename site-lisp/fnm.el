;;; fnm.el --- Emacs Lisp FNM wrapper  -*- lexical-binding: t -*-

;; Author: Adam Bobrow
;; Maintainer: Adam Bobrow
;; Version: version
;; Package-Requires: ((s)
;;                    (cl)
;;                    (eshell))

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

;; commentary

;;; Code:
(require 's)
(require 'cl)
(require 'eshell)

(defvar fnm-dir (cadr (s-split "=" (cl-find-if
                                    (lambda (s) (s-starts-with-p "FNM_DIR" s))
                                    (s-split "\n" (shell-command-to-string "zsh; eval \"$(fnm env --use-on-cd)\"; env | rg FNM"))))))

(defun fnm-npm-path (node-version)
  "Return the path to the npm binary for the given NODE-VERSION."
  (let ((node-path-request (split-string (shell-command-to-string (format "zsh; eval \"$(fnm env --use-on-cd)\; fnm use %s; which npm\""
                                                                          node-version))
                                         "\n")))
            (when (s-starts-with-p "error" (car node-path-request))
              (error "Node version %s is not currently installed by FNM" node-version))
            (car (last (remove "" node-path-request)))))

(defun fnm-node-path (node-version)
  "Return the node path to the given NODE-VERSION."
  (let ((node-path-request (split-string (shell-command-to-string (format "zsh; eval \"$(fnm env --use-on-cd)\; fnm use %s; which node\""
                                                                          node-version))
                                         "\n")))
            (when (s-starts-with-p "error" (car node-path-request))
              (error "Node version %s is not currently installed by FNM" node-version))
            (car (last (remove "" node-path-request)))))

(defun fnm-nodemon-path (node-version)
  "Return the path to the nodemon binary for the given NODE-VERSION."
  (let ((node-path-request (split-string (shell-command-to-string (format "zsh; eval \"$(fnm env --use-on-cd)\; fnm use %s; which nodemon\""
                                                                          node-version))
                                         "\n")))
    (when (s-starts-with-p "error" (car node-path-request))
      (error "Node version %s is not currently installed by FNM" node-version))
    (cadr node-path-request)))

(defun node--version-is-not-installed-p (fnm-env-string)
  "Return t if the given FNM-ENV-STRING indicates that the node version is not installed."
  (s-contains? "is not currently installed" fnm-env-string))

(defun fnm-node-bin-path (node-version)
  "Return the bin path the given NODE-VERSION."
  (s-replace "/node" "" (fnm-node-path node-version)))

(defun fnm-node-modules-path (node-version)
  "Return the node modules path for the given NODE-VERSION."
  (s-replace "/bin/node" "/lib/node_modules/" (fnm-node-path node-version)))

;;;###autoload
(defun fnm-use (node-version)
  "Use the given NODE-VERSION.
This function will set the node version to the given NODE-VERSION
and update the PATH variable to include the path to the node binary."
  (interactive (list (completing-read "sNode version: " (get-available-fnm-node-versions))))
  (setenv "PATH" (concat (fnm-node-bin-path node-version)
                         ":"
                         (s-join ":" (remove-if
                           (lambda (s) (s-contains-p "fnm_multishells" s))
                           (s-split ":" (getenv "PATH"))))))
  (setenv "NODE_PATH" (fnm-node-modules-path node-version))
  (message "Now using node version %s" node-version))

(defun get-available-fnm-node-versions ()
  "Return a list of available fnm node versions."
  (remove-if 'nil
             (mapcar 
              (lambda (s) (nth 1 (s-match "\\(v.+?\\)\\( default\\|$\\)" s)))
              (s-split "\n" (shell-command-to-string "zsh; eval \"$(fnm env --use-on-cd)\; fnm list;\"")))))

(provide 'fnm)
;;; fnm.el ends here
