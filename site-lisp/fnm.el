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
  "The defined default FNM node version"
      (car (s-split "\n" (shell-command-to-string "zsh; eval \"$(fnm env --use-on-cd)\"; node -v"))))

(defvar fnm-dir (cadr (s-split "=" (cl-find-if
                                    (lambda (s) (s-starts-with-p "FNM_DIR" s))
                                    (s-split "\n" (shell-command-to-string "zsh; eval \"$(fnm env --use-on-cd)\"; env | rg FNM"))))))

(defvar fnm-node (concat fnm-node-path
                       "/node"))
(defvar fnm-npm (concat fnm-dir
                      "/node-versions/" default-node-version "/installation/bin/npm"))
(defvar lsp-clients-typescript-npm-location
        fnm-npm)


(defun fnm-node-path (node-version)
  (format "%s/bin/node"
          (let ((exports (split-string (shell-command-to-string (format "zsh; eval \"$(fnm env --use-on-cd)\; fnm use %s; $(print fnm env)\""
                                                                        node-version))
                                       "\n")))
            (when (node--version-is-not-installed-p (car exports))
              (error "Node version %s is not currently installed by FNM" node-version))
            (loop for export in exports
                  for env = (s-split "=" (s-chop-prefix "export " export))
                  when (equal (car env) "FNM_MULTISHELL_PATH")
                  return (s-replace "\"" "" (cadr env))))))

(defun node--version-is-not-installed-p (fnm-env-string)
  (s-contains? "is not currently installed" fnm-env-string))

(fnm-node-path 18)"/Users/bob/Library/Caches/fnm_multishells/44633_1672357307514/bin/node"
(fnm-node-path 12)"/Users/bob/Library/Caches/fnm_multishells/44874_1672357329659/bin/node"
(fnm-node-path 8)
(fnm-node-path 123)


(provide 'fnm)

;;; fnm.el ends here
