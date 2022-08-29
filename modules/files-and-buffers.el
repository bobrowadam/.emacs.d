;; Dired
(use-package dired
  :ensure nil
  :demand t
  :config
  (setq dired-use-ls-dired nil)
  ;; (setq dired-listing-switches "-alh")
  (setq insert-directory-program (s-replace "\n" "" (s-replace "//" "/" (shell-command-to-string "which gls"))))
  :hook (dired-mode . (lambda () (dired-hide-details-mode 1))))

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
  (defun dirvish-find-entry-ad (&optional entry)
    "Find file in dirvish buffer.
ENTRY can be a filename or a string with format of
`dirvish-fd-bufname' used to query or create a `fd' result
buffer, it defaults to filename under the cursor when it is nil."
    (let* ((entry (or entry (dired-get-filename nil t)))
           (dv (or (dirvish-curr) (user-error "Not in a dirvish session")))
           (buffer (dirvish--find-entry dv entry)))
      (if buffer
          (dirvish-with-no-dedication (switch-to-buffer buffer))
        (find-file entry))))
  (require 'dirvish-fd)
  (dirvish-define-preview exa (file)
                          "Use `exa' to generate directory preview."
                          :require ("exa") ; tell Dirvish to check if we have the executable
                          (when (file-directory-p file) ; we only interest in directories here
                            `(shell . ("exa" "--color=always" "-al" ,file))))
  (add-to-list 'dirvish-preview-dispatchers 'exa)
  (dirvish-define-preview bat (file)
    "Use `bat' to generate directory preview."
    :require ("bat") ; tell Dirvish to check if we have the executable
    (when (not (file-directory-p file)) ; we only interest in directories here
      `(shell . ("bat" "--color=always" "--decorations=always" ,file))))
  (add-to-list 'dirvish-preview-dispatchers 'bat)

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
   ("M-j" . dirvish-fd)))

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

(defun ibuffer-filter-by-prog-mode  ()
  (ibuffer-filter-by-derived-mode 'prog-mode))

(defun short--file-path (file-path)
  (s-prepend "/" (s-join "/" (-take-last 4 (s-split "/" file-path)))))

(use-package project-ibuffer
  :after (ibuffer)
  :ensure nil
  :bind
  ("C-x p I" . project-ibuffer))

(use-package ibuffer
  :ensure nil
  :bind
  ("C-x C-b" . ibuffer)


  :config
  (define-ibuffer-column short-file-name (:name Testing-Define-Column :inline true)
    (if-let ((root-dir (cdr (ibuffer-vc-root (current-buffer))))
             (visiting-file-name (buffer-file-name)))
        (short--file-path (s-replace (expand-file-name root-dir) "" visiting-file-name))
      (or (buffer-file-name) (buffer-name))))
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (cond
     ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
     ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
     (t (format "%8d" (buffer-size)))))
  :custom
  (ibuffer-expert t)
  (ibuffer-show-empty-filter-groups nil)
  (ibuffer-formats
   '((mark modified read-only vc-status-mini " "
           (short-file-name))
     (mark modified read-only vc-status-mini " "
           (name 18 18 :left :elide)
           " "
           (size-h 9 -1 :right)
           " "
           (mode 16 16 :left :elide)
           " "
           filename-and-process)))
  (ibuffer-filter-group-name-face 'font-lock-doc-face)
  :hook
  (ibuffer . (lambda ()
               (ibuffer-auto-mode)
               (ibuffer-vc-set-filter-groups-by-vc-root)
               (ibuffer-filter-by-prog-mode)
               (all-the-icons-ibuffer-mode)
               (unless (eq ibuffer-sorting-mode 'recency)
                 (ibuffer-do-sort-by-recency)))))

(use-package all-the-icons-ibuffer
  :init (all-the-icons-ibuffer-mode 1))

(use-package ibuffer-vc
  :demand
  :commands (ibuffer-vc-set-filter-groups-by-vc-root)
  :custom
  (ibuffer-vc-skip-if-remote t))

(use-package csv-mode)
(use-package dockerfile-mode)
(use-package pdf-tools)

(provide 'files-and-buffers)
