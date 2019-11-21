;;; java-setup.el --- summary -*- lexical-binding: t -*-

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

(use-package lsp-java
  :after lsp
  :config 
  :hook
  (java-mode . (lambda () (yas-load-directory (concat user-emacs-directory "snippets/java-mode/"))))
  (java-mode . lsp))

(use-package dap-mode
  :after lsp-mode
  :config
  (dap-mode t)
  (dap-ui-mode t))

(use-package gradle-mode
  :demand t)

(provide 'java-setup)

;;; java-setup.el ends here
