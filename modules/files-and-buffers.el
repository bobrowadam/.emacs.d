;; Dired
(use-package dired
  :commands dired
  :ensure nil
  :custom
  (dired-auto-revert-buffer t)
  (dired-listing-switches "-lah --group-directories-first")
  (dired-use-ls-dired t)
  (delete-by-moving-to-trash t)
  (dired-dwim-target t)
  :config
  (setq insert-directory-program (s-replace "\n" "" (s-replace "//" "/" (shell-command-to-string "which gls"))))
  :hook (dired-mode . (lambda () (dired-hide-details-mode 1))))

(use-package dired-imenu
  :after dired
  :config (dired-setup-imenu))

(use-package casual-dired
  :commands (dired dired-jump)
  :after (dired)
  :bind (:map dired-mode-map
              ("C-o" . #'casual-dired-tmenu)))

(use-package dired-sidebar
  :commands (dired-sidebar-toggle-sidebar dired-sidebar-toggle-with-current-directory)
  :after (dired)
  :bind 
  ;; This can be achived using "C-u C-x C-j" so not binding it
  ;; ("C-x C-d" . dired-sidebar-toggle-with-current-directory)
  ("C-x C-j" . dired-sidebar-toggle-sidebar)
  :custom
  (dired-sidebar-width 25)
  (dired-sidebar-subtree-line-prefix "  ")
  (dired-sidebar-theme 'vscode)
  (dired-sidebar-use-term-integration t)
  (dired-sidebar-use-custom-font t)
  :hook
  (dired-sidebar-mode . (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  :custom-face
  (dired-sidebar-face ((t (:family "Menlo"))))

  ;; :config
  ;; (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  ;; (push 'rotate-windows dired-sidebar-toggle-hidden-commands)
)

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
  :config
  (add-to-list 'all-the-icons-extension-icon-alist
               '("roc" all-the-icons-fileicon "elm" :face all-the-icons-blue))
  (add-to-list 'all-the-icons-mode-icon-alist
             '(roc-mode all-the-icons-fileicon "elm" :face all-the-icons-blue))
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
  (if (one-window-p)
      (kill-buffer)
    (kill-buffer-and-window)))

(use-package basic-keybindigs
  :ensure nil
  :bind
  ;; ("C-c M-s" . isearch-forward-symbol-at-point)
  ("C-x j" . whitespace-cleanup)
  ("C-^" . (lambda () (interactive (delete-indentation -1))))
  ("M-C-h" . backward-kill-sexp)
  ("C-x -" . my/gloden-ratio)
  ("C-x f" . recentf-open-files)
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
               (ibuffer-filter-by-prog-mode)
               (all-the-icons-ibuffer-mode)
               (unless (eq ibuffer-sorting-mode 'recency)
                 (ibuffer-do-sort-by-recency))
               (ibuffer-vc-set-filter-groups-by-vc-root))))

(use-package casual-ibuffer
  :commands (ibuffer)
  :bind (:map ibuffer-mode-map
              ("C-o" . casual-ibuffer-tmenu)
              ("F" . casual-ibuffer-filter-tmenu)
              ("s" . casual-ibuffer-sortby-tmenu)))

(use-package all-the-icons-ibuffer
  :commands (ibuffer)
  :config (all-the-icons-ibuffer-mode 1)
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
  :commands (ibuffer-vc-set-filter-groups-by-vc-root)
  :custom
  (ibuffer-vc-skip-if-remote t))

(use-package csv-mode)
(use-package dockerfile-mode)
(use-package pdf-tools
  :config (pdf-tools-install))

(use-package casual-re-builder
  :commands (re-builder)
  :bind
  (:map reb-mode-map
              ("C-o" . #'casual-re-builder-tmenu))
  (:map reb-lisp-mode-map
        ("C-o" . #'casual-re-builder-tmenu)))

(provide 'files-and-buffers)
