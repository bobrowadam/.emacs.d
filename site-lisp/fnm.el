;;; fnm.el --- Emacs Lisp FNM wrapper  -*- lexical-binding: t -*-

;; Author: Adam Bobrow
;; Maintainer: Adam Bobrow
;; Version: version
;; Package-Requires: (dependencies)
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

(defvar default-node-version
  (car (s-split "\n" (shell-command-to-string "zsh; eval \"$(fnm env --use-on-cd)\"; node -v")))
  "The defined default FNM node version")

(defvar fnm-dir (cadr (s-split "=" (cl-find-if
                                    (lambda (s) (s-starts-with-p "FNM_DIR" s))
                                    (s-split "\n" (shell-command-to-string "zsh; eval \"$(fnm env --use-on-cd)\"; env | rg FNM"))))))

(defvar fnm-npm (concat fnm-dir
                      "/node-versions/" default-node-version "/installation/bin/npm"))

(defvar fnm-node-modules (concat fnm-dir
                      "/node-versions/" default-node-version "/installation/lib/node_modules"))

(defvar lsp-clients-typescript-npm-location
        fnm-npm)

(defun fnm-npm-path (node-version)
  (let ((node-path-request (split-string (shell-command-to-string (format "zsh; eval \"$(fnm env --use-on-cd)\; fnm use %s; which npm\""
                                                                          node-version))
                                         "\n")))
            (when (s-starts-with-p "error" (car node-path-request))
              (error "Node version %s is not currently installed by FNM" node-version))
            (car (last (remove "" node-path-request)))))

(defun fnm-node-path (node-version)
  (let ((node-path-request (split-string (shell-command-to-string (format "zsh; eval \"$(fnm env --use-on-cd)\; fnm use %s; which node\""
                                                                          node-version))
                                         "\n")))
            (when (s-starts-with-p "error" (car node-path-request))
              (error "Node version %s is not currently installed by FNM" node-version))
            (car (last (remove "" node-path-request)))))

(defun fnm-nodemon-path (node-version)
  (let ((node-path-request (split-string (shell-command-to-string (format "zsh; eval \"$(fnm env --use-on-cd)\; fnm use %s; which nodemon\""
                                                                          node-version))
                                         "\n")))
    (when (s-starts-with-p "error" (car node-path-request))
      (error "Node version %s is not currently installed by FNM" node-version))
    (cadr node-path-request)))


(defun node--version-is-not-installed-p (fnm-env-string)
  (s-contains? "is not currently installed" fnm-env-string))

(defun fnm-use (node-version)
  (cond ((equal major-mode 'eshell-mode)
         (eshell/alias "node" (fnm-node-path node-version) "$1"))))

(provide 'fnm)
;;; fnm.el ends here


;; (setq default-node-version
;;       (car (s-split "\n" (shell-command-to-string "zsh; eval \"$(fnm env --use-on-cd)\"; node -v"))))
;; (setq fnm-dir (cadr (s-split "=" (cl-find-if
;;                                   (lambda (s) (s-starts-with-p "FNM_DIR" s))
;;                                   (s-split "\n" (shell-command-to-string "zsh; eval \"$(fnm env --use-on-cd)\"; env | rg FNM"))))))
;; (setq fnm-node-path (concat fnm-dir
;;                             "/node-versions/" default-node-version "/installation/bin"))

;; (setq fnm-node (concat fnm-node-path
;;                        "/node"))
;; (setq fnm-npm (concat fnm-dir
;;                       "/node-versions/" default-node-version "/installation/bin/npm"))
