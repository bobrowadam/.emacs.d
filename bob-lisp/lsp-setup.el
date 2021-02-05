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
  :demand
  :commands lsp
  :init
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  (defvar lsp-print-io t)
  (defvar lsp-scala-server-command "/usr/local/bin/metals-emacs")
  (defvar lsp-prefer-flymake nil)
  :hook
  (lsp-mode . origami-mode)
  (lsp-after-initialize . (lambda ()
                            (progn
                              (flycheck-mode +1)
                              (when (equal major-mode 'typescript-mode)
                                (flycheck-add-next-checker 'lsp 'javascript-eslint)
                                (flycheck-select-checker 'javascript-eslint))
                              (when (equal major-mode 'js2-mode)
                                (flycheck-select-checker 'javascript-eslint))
                              )))
  :config
  (setq lsp-completion-provider :capf)
  (setq lsp-idle-delay 0.500)
  (setenv "BOB_DIR" (format "%s%s" (getenv "HOME") "/source/bob"))
  (exec-path-from-shell-copy-envs '("WHATSAPP_NUMBER"))
  (exec-path-from-shell-copy-envs '("LOCAL_WHATSAPP_NUMBER"))
  :bind
  (:map lsp-mode-map
        ("C-c C-r" . lsp-find-references)
        ("M-." . lsp-find-definition)
        ("C-c M-d" . lsp-describe-thing-at-point)
        ("C-c C-f" . lsp-format-buffer)
        ("C-c C-n" . lsp-rename)
        ("C-c d" . dap-hydra)))

(use-package lsp-ui
  :config
  (setq
   lsp-ui-doc-enable nil
   lsp-ui-peek-enable nil
   lsp-ui-sideline-enable nil)
  :bind
  (:map lsp-mode-map
        ("C-c C-." . lsp-ui-sideline-toggle-symbols-info)
        ("C-c M-i" . lsp-ui-imenu)
        ))

(use-package lsp-origami :ensure t
  :bind ("C-=" . origami-toggle-node))
(use-package company-lsp :ensure t)
(provide 'lsp-setup)

;;; ls-setup.el ends here
