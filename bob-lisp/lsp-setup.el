;;; ls-setup.el --- summary -*- lexical-binding: t -*-

;; Author: adam bob
;; Maintainer: adam bob
;; Version: 1.0
;; Package-Requires: (lsp-mode)
;; Homepage: none
;; Keywords: nil


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

;; LSP setup for various langs

;;; Code:

(use-package lsp-mode
  ;; :demand t
  :commands lsp
  :init
  (defvar lsp-print-io t)
  (defvar lsp-scala-server-command "/usr/local/bin/metals-emacs")
  (defvar lsp-prefer-flymake nil)
  :config
  (require 'lsp-clients)
  :hook lsp-origami-mode
  :bind
  (:map lsp-mode-map
        ("C-c C-." . lsp-ui-sideline-toggle-symbols-info)
        ("C-c C-r" . lsp-find-references)
        ("C-c M-i" . lsp-ui-imenu)
        ("C-c M-d" . lsp-describe-thing-at-point)
        ("C-c C-f" . lsp-format-buffer)
        ("C-=" . origami-toggle-node)))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq
   lsp-ui-doc-enable t
   lsp-ui-peek-enable t
   lsp-ui-sideline-enable t))

(use-package lsp-origami :ensure t)
(use-package company-lsp :ensure t)
(provide 'lsp-setup)

;;; ls-setup.el ends here
