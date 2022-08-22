;; Dired
(use-package dired
  :demand t
  :config
  (setq dired-use-ls-dired nil)
  ;; (setq dired-listing-switches "-alh")
  (setq insert-directory-program (s-replace "\n" "" (s-replace "//" "/" (shell-command-to-string "which gls"))))
  :hook (dired-mode . (lambda () (dired-hide-details-mode 1)))
  :ensure nil)

(use-package diredfl
  :hook
  (dired-mode . diredfl-mode))

(use-package dired-rsync
  :init
  (setq dired-rsync-passphrase-stall-regex "Verification code")
  :bind (:map dired-mode-map ("C-c C-r" . dired-rsync)))

(use-package dirvish
  :demand t
  :after (dired)
  :ensure t
  :custom
  (dirvish-quick-access-entries
   '(("h" "~/"                          "Home")
     ("d" "~/Downloads/"                "Downloads")
     ("s" "~/source/services" "Services")
     ("S" "~/source/"                "Source")))
  (dirvish-header-line-format '(:left (path fd-pwd) :right (free-space)))
  (dirvish-mode-line-format ; it's ok to place string inside
   '(:left (sort file-time " " vc-info fd-pwd)  :right (omit yank index)))
  
  ;; In case you want the details at startup like `dired'
  (dirvish-hide-details nil)
  :init
  ;; Let Dirvish take over Dired globally
  (setq dirvish-attributes '(all-the-icons collapse subtree-state vc-state))
  :config
  (require 'dirvish-fd)
  (dirvish-define-preview exa (file)
                          "Use `exa' to generate directory preview."
                          :require ("exa") ; tell Dirvish to check if we have the executable
                          (when (file-directory-p file) ; we only interest in directories here
                            `(shell . ("exa" "--color=always" "-al" ,file))))
  (add-to-list 'dirvish-preview-dispatchers 'exa)
  (dirvish-override-dired-mode)
    (dirvish-peek-mode)
  ;; Dired options are respected except a few exceptions, see *In relation to Dired* section above
  (setq dired-dwim-target t)
  (setq delete-by-moving-to-trash t)
  :bind
  (("C-c C-l" . dirvish-side)
   ("C-x C-d" . dirvish)
   :map dirvish-mode-map
   ("TAB" . dirvish-subtree-toggle)
   ("C-x C-j" . dired-up-directory)
   ("a"   . dirvish-quick-access)
   ("f"   . dirvish-file-info-menu)
   ("y"   . dirvish-yank-menu)
   ("N"   . dirvish-narrow)
   ("^"   . dirvish-history-last)
   ("h"   . dirvish-history-jump)  ; remapped `describe-mode'
   ("s"   . dirvish-quicksort) ; remapped `dired-sort-toggle-or-edit'
   ("M-n" . dirvish-history-go-forward)
   ("M-p" . dirvish-history-go-backward)
   ("M-l" . dirvish-ls-switches-menu)
   ("M-m" . dirvish-mark-menu)
   ("M-f" . dirvish-layout-toggle)
   ("M-s" . dirvish-setup-menu)
   ("M-e" . dirvish-emerge-menu)
   ("M-j" . dirvish-fd-jump)))

;; setup
(defmacro csetq (variable value)
  `(funcall (or (get ',variable 'custom-set)
                'set-default)
            ',variable ,value))
(csetq ediff-window-setup-function 'ediff-setup-windows-plain)
(csetq ediff-split-window-function 'split-window-horizontally)
(csetq ediff-diff-options "-w")
(defun ediff-copy-both-to-C ()
  (interactive)
  (ediff-copy-diff ediff-current-difference nil 'C nil
                   (concat
                    (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                    (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
(defun add-d-to-ediff-mode-map () (define-key ediff-mode-map "d" 'ediff-copy-both-to-C))
(add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map)

(defun bob/kill-this-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))

(use-package basic-keybindigs
  :ensure nil
  :bind
  ;; ("C-c M-s" . isearch-forward-symbol-at-point)
  ("C-x j" . whitespace-cleanup)
  ("C-^" . (lambda () (interactive (delete-indentation -1))))
  ("M-C-h" . backward-kill-sexp)
  ("C-x -" . my/gloden-ratio)
  ("C-x f" . recentf-open-files)
  ("M-o" . other-frame)
  ("C-x k" . bob/kill-this-buffer)
  ("M-SPC" . cycle-spacing)
  ("<s-return>" . toggle-frame-fullscreen))

(use-package golden-ratio
  :init (defun my/gloden-ratio ()
          "Toggle golden ratio"
          (interactive)
          (if golden-ratio-mode
              (progn (golden-ratio-mode -1)
                     (balance-windows))
            (progn (golden-ratio-mode)
                   (golden-ratio))))
  :config (add-to-list 'golden-ratio-extra-commands 'ace-window))

(use-package csv-mode)
(use-package dockerfile-mode)
(use-package pdf-tools)

(provide 'files-and-buffers)
