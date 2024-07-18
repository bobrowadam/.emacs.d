;; Dired
(use-package dired
  :ensure nil
  :demand t
  :custom
  (dired-auto-revert-buffer t)
  (dired-listing-switches "-lah --group-directories-first")
  (delete-by-moving-to-trash t)
  (dired-use-ls-dired nil)
  (insert-directory-program (s-replace "\n" "" (s-replace "//" "/" (shell-command-to-string "which gls"))))
  (dired-dwim-target t)
  :hook (dired-mode . (lambda () (dired-hide-details-mode 1))))

(use-package casual-dired
  :bind (:map dired-mode-map
              ("C-o" . #'casual-dired-tmenu)
              ("s" . #'casual-dired-sort-by-tmenu)))

(use-package dired-sidebar
  :after (dired)
  :bind (("C-x C-d" . dired-sidebar-toggle-sidebar))
  :ensure t
  :commands (dired-sidebar-toggle-sidebar)
  :init
  (setq dired-sidebar-face `(:family "Menlo"))
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)
  (setq dired-sidebar-subtree-line-prefix " ")
  (setq dired-sidebar-theme 'vscode)
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-font t))

(use-package dired-subtree
  :demand t
  :after (dired)
  :bind (:map dired-mode-map
              ("TAB" . dired-subtree-toggle)))
(use-package all-the-icons-dired
  :demand t
  :custom
  (all-the-icons-dired-monochrome nil)
  :after (dired)
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package diredfl
  :hook
  (dired-mode . diredfl-mode))

(use-package dired-rsync
  :custom
  (dired-rsync-options "-az --progress")
  :init
  (setq dired-rsync-passphrase-stall-regex "^.+password: \\|Verification code.*"))

(defun ediff-copy-both-to-C ()
  (interactive)
  (ediff-copy-diff ediff-current-difference nil 'C nil
                   (concat
                    (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                    (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
(defun add-d-to-ediff-mode-map ()
    (define-key ediff-mode-map "d" 'ediff-copy-both-to-C))

(use-package ediff
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-diff-options "-w")
  :hook (ediff-keymap-setup . #'add-d-to-ediff-mode-map))

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
  :demand t
  :bind
  ("C-x C-b" . ibuffer)
  :custom
  (ibuffer-expert t)
  (ibuffer-use-other-window t)
  (ibuffer-show-empty-filter-groups nil)
  (ibuffer-filter-group-name-face 'font-lock-doc-face)
  :hook
  (ibuffer . (lambda ()
               (ibuffer-auto-mode)
               (ibuffer-vc-set-filter-groups-by-vc-root)
               (ibuffer-filter-by-prog-mode)
               (all-the-icons-ibuffer-mode)
               (unless (eq ibuffer-sorting-mode 'recency)
                 (ibuffer-do-sort-by-recency)))))

(use-package casual-ibuffer
  :after ibuffer
  :bind (:map ibuffer-mode-map
              ("C-o" . casual-ibuffer-tmenu)
              ("F" . casual-ibuffer-filter-tmenu)
              ("s" . casual-ibuffer-sortby-tmenu)))

(use-package all-the-icons-ibuffer
  :demand t
  :init (all-the-icons-ibuffer-mode 1)
  :custom (all-the-icons-ibuffer-formats '((mark modified read-only locked " "
                                                 (icon 2 2)
                                                 (name 16 -1 :left)
                                                 " " filename-and-process+)
                                           (mark modified read-only locked " "
                                                 (icon 2 2)
                                                 (name 18 18 :left :elide)
                                                 " "
                                                 (size-h 9 -1 :right)
                                                 " "
                                                 (mode+ 16 16 :left :elide)
                                                 " " filename-and-process+)
                                           )))

(use-package ibuffer-vc
  :demand
  :commands (ibuffer-vc-set-filter-groups-by-vc-root)
  :custom
  (ibuffer-vc-skip-if-remote t))

(use-package csv-mode)
(use-package dockerfile-mode)
(use-package pdf-tools)

(use-package casual-re-builder
  :commands (re-builder)
  :bind
  (:map reb-mode-map
              ("C-o" . #'casual-re-builder-tmenu))
  (:map reb-lisp-mode-map
        ("C-o" . #'casual-re-builder-tmenu)))

(provide 'files-and-buffers)
