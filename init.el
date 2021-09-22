(setq gc-cons-threshold 100000000)
(setq debug-on-error nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)
            ))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(unless (not (file-exists-p custom-file))
  (load custom-file))

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq package-native-compile t)
(setq use-package-always-defer t)
(setq use-package-always-ensure t)
(setq use-package-verbose t)
(setq use-package-debug t)
(setq use-package-compute-statistics t)

(require 'server)
(unless (server-running-p)
  (server-start))

(use-package exec-path-from-shell
  :if (window-system)
  :demand
  :config
  (add-to-list 'exec-path-from-shell-variables "BOB_DIR")
  (add-to-list 'exec-path-from-shell-variables "WHATSAPP_NUMBER")
  (add-to-list 'exec-path-from-shell-variables "LOCAL_WHATSAPP_NUMBER")
  (exec-path-from-shell-initialize)
  (setq service-directory (concat (getenv "HOME") "/source/services")))


(setq user-login-name "Adam Bobrow"
      make-backup-files nil
      enable-recursive-minibuffers t
      inhibit-splash-screen t
      require-final-newline nil
      truncate-partial-width-windows 80
      sentence-end-double-space t ; explicitly choose default
      x-select-enable-clipboard t
      set-mark-command-repeat-pop t

      history-delete-duplicates t
      comint-input-ignoredups t

      view-read-only nil ; all read-only buffers in view-mode
      view-inhibit-help-message t ; don't tell me about it

      delete-active-region nil ; just use <delete>

      gdb-many-windows t

      epa-pinentry-mode 'loopback
      auth-sources '("~/.authinfo" "~/.authinfo.gpg" "~/.netrc")

      ;; No more damn prompts!
      dired-recursive-deletes 'always
      dired-recursive-copies 'always
      ;; blink-cursor-blinks 1
      )

(setq initial-scratch-message ";; Oh it's you again :|")
(setq scroll-conservatively 101
      scroll-margin 2)
(setq display-time-day-and-date t)
(setq display-time-24hr-format t)
(setq display-time-default-load-average nil)
(setq shift-select-mode nil)
(display-time)
(display-battery-mode)
(menu-bar-mode -1)
(column-number-mode 1)
(when (window-system)
  (setq confirm-kill-emacs 'yes-or-no-p))

(when window-system
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1))

(set-default 'cursor-type  '(bar . 10))
(blink-cursor-mode 0)

(global-subword-mode t)
(global-superword-mode -1)
(setq inhibit-startup-message t)
(setq ring-bell-function 'ignore
      visible-bell nil)
(setq-default indent-tabs-mode nil)

(delete-selection-mode 1)
(set-default 'indent-tabs-mode nil)
(global-auto-revert-mode 1)
(auto-compression-mode t)
(defalias 'yes-or-no-p 'y-or-n-p)

;; delete char and delte word with "C-h" "C-M-h"
(define-key key-translation-map [?\C-h] [?\C-?])
(define-key key-translation-map (kbd "<f1>") (kbd "C-h"))
;; (define-key key-translation-map (kbd "<f9>") (kbd "C-s-h"))

;; Deal with editing large files:
(global-so-long-mode 1)

;; Theme and Font
(setq custom-safe-themes t)
(setq custom-theme-directory "~/.emacs.d/themes")
(set-frame-font "Roboto Mono 23")
(add-to-list 'default-frame-alist
             '(font . "Roboto Mono 23"))

(use-package doom-modeline
  :if (window-system)
  :after ivy
  :demand t
  :config
  (setq doom-modeline-height 20)
  (setq doom-modeline-bar-width 2)
  (setq doom-modeline-minor-modes nil)
  (setq doom-modeline-lsp t)
  (setq doom-modeline-github t)
  ;; (setq doom-modeline-env-enable-rust t)
  ;; (setq doom-modeline-env-rust-executable "rustc")
  (setq find-file-visit-truename t)
  (doom-modeline-mode 1)
  (custom-theme-set-faces 'user '(ivy-current-match ((t (:extend t :background "gray27")))))
  )

(use-package doom-themes
  :if (window-system)
  :demand t
  :config
  (load-theme 'doom-ayu-mirage t)
  ;; (load-theme 'doom-monokai-spectrum t)
  ;; (load-theme 'doom-old-hope t)
  ;; (load-theme 'doom-oceanic-next t)
  ;; (load-theme 'doom-acario-dark t)
  ;; (load-theme 'doom-Iosvkem t)
  ;; (load-theme 'doom-moonlight t)
  ;; (load-theme 'bobs-badger)
  ;; (load-theme 'modus-vivendi)
  ;; (load-theme 'doom-gruvbox)
  ;; (load-theme 'doom-ir-black)
  (setq doom-themes-treemacs-theme "doom-colors"))

(use-package tron-legacy-theme)

(use-package night-owl-theme)

;; Put backup files neatly away
(let ((backup-dir "~/tmp/emacs/backups")
      (auto-saves-dir "~/tmp/emacs/auto-saves/"))
  (dolist (dir (list backup-dir auto-saves-dir))
    (when (not (file-directory-p dir))
      (make-directory dir t)))
  (setq backup-directory-alist `(("." . ,backup-dir))
        auto-save-file-name-transforms `((".*" ,auto-saves-dir t))
        auto-save-list-file-prefix (concat auto-saves-dir ".saves-")
        tramp-backup-directory-alist `((".*" . ,backup-dir))
        tramp-auto-save-directory auto-saves-dir))

(setq backup-by-copying t    ; Don't delink hardlinks
      delete-old-versions t  ; Clean up the backups
      version-control t      ; Use version numbers on backups,
      kept-new-versions 5    ; keep some new versions
      kept-old-versions 2)   ; and some old ones, too

(add-to-list 'prog-mode-hook #'linum-mode)
(setq linum-format "%4d   ")
;; Set Emacs C source dir:
(setq find-function-C-source-directory "~/source/emacs/src")

(use-package dired
  :config
  (setq dired-use-ls-dired nil)
  (setq dired-listing-switches "-alh")
  :ensure nil
  )

(use-package dired-x :ensure nil :defer 1)

(use-package dired-aux
  :ensure nil
  :after (dired)
  :config
  (setq dired-isearch-filenames 'dwim)
  (setq dired-create-destination-dirs 'ask)
  (setq dired-vc-rename-file t)
  :bind (:map dired-mode-map
              ("C-+" . dired-create-empty-file)
              ("M-s f" . nil)))

(use-package dired-subtree
  :after (dired)
  :ensure t
  :bind (:map dired-mode-map
              ("<tab>" . dired-subtree-toggle)
              ("<C-tab>" . dired-subtree-cycle)
              ("<backtab>" . dired-subtree-remove)))

(use-package diredfl
  :ensure t
  :init (diredfl-global-mode))

(use-package dired-rsync
  :init
  (setq dired-rsync-passphrase-stall-regex "Verification code")
  :bind (:map dired-mode-map ("C-c C-r" . dired-rsync)))

(use-package dired-du)
(use-package dired-sidebar
  :bind (("C-c C-l" . dired-sidebar-toggle-sidebar))
  :ensure nil
  :commands (dired-sidebar-toggle-sidebar))

;; Ediff setup
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

(use-package basic-keybindigs
  :ensure nil
  :bind
  ("C-c M-s" . isearch-forward-symbol-at-point)
  ("M-i" . counsel-imenu)
  ("C-x j" . whitespace-cleanup)
  ("C-^" . (lambda () (interactive (delete-indentation -1))))
  ("M-C-h" . backward-kill-sexp)
  ("C-x -" . my/gloden-ratio)
  ("C-x f" . recentf-open-files)
  ("M-o" . other-frame)
  ("C-x k" . kill-this-buffer)
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

(use-package which-key
  :demand t
  :if (window-system)
  :config
  (which-key-mode 1))

(use-package add-node-modules-path)
(use-package flycheck)
(use-package flycheck-posframe
  :ensure t
  :after flycheck
  :config (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode))

(use-package rust-mode)
(use-package cargo-mode)

(use-package typescript-mode
  :init
  (defun lsp-ts-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-eslint-apply-all-fixes))
  :hook
  (typescript-mode . lsp-ts-install-save-hooks)
  (typescript-mode . add-node-modules-path)
  (typescript-mode . origami-mode)
  :config
  (setq typescript-indent-level 2))

(use-package jest-test-mode 
  :commands jest-test-mode
  :hook (typescript-mode js-mode typescript-tsx-mode))

(use-package js2-mode
  :init
  (defun lsp-js-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-eslint-apply-all-fixes))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  :hook
  (js2-mode . add-node-modules-path)
  (js2-mode . lsp-js-install-save-hooks)
  (js2-mode . lsp-deferred)
  (js2-mode . js2-imenu-extras-mode)
  (js2-mode . js2-mode-hide-warnings-and-errors)
  (js2-mode . electric-indent-mode)
  (js2-mode . origami-mode)
  :bind (:map js2-mode-map
              ("C-<tab>" . js2-indent-bounce)
              ("C-c C-s" . nil)
              ("C-x C-e" . js-send-last-sexp))
  :config
  (setq-default js2-auto-indent-p nil)
  (setq-default unset-electric-indent)
  (setq-default js-indent-level 2))

(use-package dap-mode
  :custom
  (dap-auto-configure-features '())
  (dap-ui-variable-length 80)
  :config
  (require 'dap-node)
  (dap-node-setup)
  (dap-register-debug-template
   "TS:Launch"
   (list :type "node"
         :request "launch"
         :smartStep t
         :skipFiles ["<node_internals>/**"]
         :outFiles ["${workspaceFolder}/dist/**/*.js"]
         :sourceMap t
         :name "TS::Run"))
  (dap-register-debug-template
   "Node:attach"
   (list :type "node"
         :request "attach"
         :skipFiles ["<node_internals>/**"]
         :name "Node-attach"))
  (dap-register-debug-template
   "TS:attach"
   (list :name "TS Index"
         :type "node"
         :skipFiles ["<node_internals>/**"]
         :outFiles ["${workspaceFolder}/dist/**/*.js"]
         :request "attach"
         :sourceMaps t))
  (dap-register-debug-template
   "NPM:start"
   (list :name "Node:start"
         :type "node"
         :skipFiles ["<node_internals>/**"]
         :request "launch"
         :runtimeArgs ["./node_modules/env-setter/src/ssm-entrypoint-local.js"]
         :sourceMaps t))
  :bind
  (:map dap-mode-map
        ("C-c d" . dap-hydra)))

(use-package dap-ui
  :ensure nil
  :after (dap-mode)
  :config
  (setq dap-ui-buffer-configurations
        `((,dap-ui--locals-buffer . ((side . right) (slot . 1) (window-width . 0.5)))
          (,dap-ui--expressions-buffer . ((side . right) (slot . 2) (window-width . 0.20)))
          (,dap-ui--sessions-buffer . ((side . right) (slot . 3) (window-width . 0.20)))
          (,dap-ui--breakpoints-buffer . ((side . left) (slot . 2) (window-width . ,treemacs-width)))
          (,dap-ui--debug-window-buffer . ((side . bottom) (slot . 3) (window-width . 0.20)))

          (,dap-ui--repl-buffer . ((side . bottom) (slot . 1) (window-height . 0.45))))))

(use-package lsp-mode
  :commands lsp
  :init
  (setq lsp-disabled-clients '((ts-mode . (eslint)) (js-mode . (eslint))))
  :custom
  (lsp-auto-guess-root nil)
  (lsp-prefer-flymake nil)           ; Use flycheck instead of flymake
  (lsp-file-watch-threshold 2000)
  (read-process-output-max (* 1024 1024))
  (lsp-eldoc-hook nil)
  (company-lsp-cache-candidates t)
  :bind
  (:map lsp-mode-map
        ("C-c C-f" . lsp-format-buffer)
        ("C-c C-n" . lsp-rename)
        ("C-c C-r" . lsp-ui-peek-find-references)
        ("M-." . lsp-ui-peek-find-definitions))
  :hook ((js2-mode typescript-mode web-mode
                   c-mode c++-mode rust-mode haskell-mode) . lsp))

(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :custom-face
  (lsp-ui-doc-background ((t (:background nil))))
  (lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic)))))
  :bind
  (:map lsp-ui-mode-map
        ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
        ([remap xref-find-references] . lsp-ui-peek-find-references)
        ("C-c u" . lsp-ui-imenu)
        ("C-c M-i" . lsp-ui-doc-focus-frame))
  (:map lsp-mode-map
        ("M-n" . forward-paragraph)
        ("M-p" . backward-paragraph))
  :custom
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-border (face-foreground 'default))
  (lsp-ui-sideline-enable nil)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-code-actions nil)
  :config
  ;; Use lsp-ui-doc-webkit only in GUI
  (if (display-graphic-p)
      (setq lsp-ui-doc-use-webkit t))
  ;; WORKAROUND Hide mode-line of the lsp-ui-imenu buffer
  ;; https://github.com/emacs-lsp/lsp-ui/issues/243
  (defadvice lsp-ui-imenu (after hide-lsp-ui-imenu-mode-line activate)
    (setq mode-line-format nil)))

(use-package company-box
  :diminish
  :if (display-graphic-p)
  :defines company-box-icons-all-the-icons
  :hook (company-mode . company-box-mode)
  :custom
  (company-box-backends-colors nil)
  :config
  (with-no-warnings
    ;; Prettify icons
    (defun my-company-box-icons--elisp (candidate)
      (when (derived-mode-p 'emacs-lisp-mode)
        (let ((sym (intern candidate)))
          (cond ((fboundp sym) 'Function)
                ((featurep sym) 'Module)
                ((facep sym) 'Color)
                ((boundp sym) 'Variable)
                ((symbolp sym) 'Text)
                (t . nil)))))
    (advice-add #'company-box-icons--elisp :override #'my-company-box-icons--elisp))

  (when (and (display-graphic-p)
             (require 'all-the-icons nil t))
    (declare-function all-the-icons-faicon 'all-the-icons)
    (declare-function all-the-icons-material 'all-the-icons)
    (declare-function all-the-icons-octicon 'all-the-icons)
    (setq company-box-icons-all-the-icons
          `((Unknown . ,(all-the-icons-material "find_in_page" :height 0.8 :v-adjust -0.15))
            (Text . ,(all-the-icons-faicon "text-width" :height 0.8 :v-adjust -0.02))
            (Method . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
            (Function . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
            (Constructor . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
            (Field . ,(all-the-icons-octicon "tag" :height 0.85 :v-adjust 0 :face 'all-the-icons-lblue))
            (Variable . ,(all-the-icons-octicon "tag" :height 0.85 :v-adjust 0 :face 'all-the-icons-lblue))
            (Class . ,(all-the-icons-material "settings_input_component" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-orange))
            (Interface . ,(all-the-icons-material "share" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
            (Module . ,(all-the-icons-material "view_module" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
            (Property . ,(all-the-icons-faicon "wrench" :height 0.8 :v-adjust -0.02))
            (Unit . ,(all-the-icons-material "settings_system_daydream" :height 0.8 :v-adjust -0.15))
            (Value . ,(all-the-icons-material "format_align_right" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
            (Enum . ,(all-the-icons-material "storage" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-orange))
            (Keyword . ,(all-the-icons-material "filter_center_focus" :height 0.8 :v-adjust -0.15))
            (Snippet . ,(all-the-icons-material "format_align_center" :height 0.8 :v-adjust -0.15))
            (Color . ,(all-the-icons-material "palette" :height 0.8 :v-adjust -0.15))
            (File . ,(all-the-icons-faicon "file-o" :height 0.8 :v-adjust -0.02))
            (Reference . ,(all-the-icons-material "collections_bookmark" :height 0.8 :v-adjust -0.15))
            (Folder . ,(all-the-icons-faicon "folder-open" :height 0.8 :v-adjust -0.02))
            (EnumMember . ,(all-the-icons-material "format_align_right" :height 0.8 :v-adjust -0.15))
            (Constant . ,(all-the-icons-faicon "square-o" :height 0.8 :v-adjust -0.1))
            (Struct . ,(all-the-icons-material "settings_input_component" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-orange))
            (Event . ,(all-the-icons-octicon "zap" :height 0.8 :v-adjust 0 :face 'all-the-icons-orange))
            (Operator . ,(all-the-icons-material "control_point" :height 0.8 :v-adjust -0.15))
            (TypeParameter . ,(all-the-icons-faicon "arrows" :height 0.8 :v-adjust -0.02))
            (Template . ,(all-the-icons-material "format_align_left" :height 0.8 :v-adjust -0.15)))
          company-box-icons-alist 'company-box-icons-all-the-icons)))

(use-package ivy
  :commands ivy-mode
  :hook (after-init . ivy-mode)
  :delight ivy-mode
  :bind (:map ivy-minibuffer-map
              ("C-j" . ivy-immediate-done)
              ("RET" . ivy-alt-done))
  :bind
  (("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("C-x C-m" . counsel-mark-ring)
   ("C-c C-s C-r" . counsel-rg)
   ("C-c C-s C-s" . swiper)
   ("C-c C-s C-g" . counsel-search))
  :config
  (ivy-prescient-mode t)
  (setq ivy-use-virtual-buffers t
        ivy-virtual-abbreviate 'full
        ivy-on-del-error-function nil
        ivy-use-selectable-prompt nil
        enable-recursive-minibuffers t
        ivy-re-builders-alist
        '((swiper . ivy--regex-plus)
          (swiper-isearch . ivy--regex-plus)
          (counsel-ag . ivy--regex-plus)
          (counsel-rg . ivy--regex-plus)
          (t . ivy-prescient-re-builder)
          )))

(use-package ivy-prescient
  :commands ivy-prescient-mode
  :custom
  (prescient-filter-method '(literal initialism prefix fuzzy))
  (ivy-prescient-retain-classic-highlighting t)
  (prescient-use-char-folding nil)
  :config
  (prescient-persist-mode t))

(use-package counsel
  :if (window-system))

(use-package projectile
  :demand t
  :init
  (setq projectile-completion-system 'ivy)
  :config
  (add-to-list 'projectile-globally-ignored-directories "node_modules")
  (setq projectile-switch-project-action #'projectile-dired)
  (projectile-global-mode 1)
  (unless projectile-known-projects
    (-let ((main-projects-directory (read-directory-name "Please enter main projects directory")))
      (projectile-discover-projects-in-directory main-projects-directory)))
  :bind
  (:map projectile-mode-map ("C-c p" . projectile-command-map)))

(use-package paredit
  :hook
  (clojure-mode . enable-paredit-mode)
  (cider-mode . enable-paredit-mode)
  :hook
  (eval-expression-minibuffer-setup . enable-paredit-mode)
  (emacs-lisp-mode . enable-paredit-mode)
  (slime-mode . enable-paredit-mode)
  (slime-repl-mode . enable-paredit-mode)
  :bind
  (:map paredit-mode-map ("C-'" . sp-rewrap-sexp)))

(use-package smartparens
  :demand t
  :init
  (setq sp-ignore-modes-list '(minibuffer-inactive-mode emacs-lisp-mode eval-expression-minibuffer-setup))
  :config
  (require 'smartparens-config)
  (smartparens-global-mode)
  (sp-local-pair 'typescript-mode "<" ">" :trigger-wrap "<")
  ;; :hook
  ;; (typescript-mode . smartparens-global-mode)
  ;; (js2-mode . smartparens-global-mode)
  :bind (:map smartparens-mode-map
              ("M-(" . sp-wrap-round)
              ("M-s" . sp-unwrap-sexp)
              ("C-)" . sp-forward-slurp-sexp)
              ("C-}" . sp-forward-barf-sexp)
              ("C-{" . sp-backward-barf-sexp)
              ("C-(" . sp-backward-slurp-sexp)
              ("C-'" . sp-rewrap-sexp)
              ("M-S" . sp-split-sexp)
              ("M-J" . sp-join-sexp)
              ("M-W" . sp-copy-sexp)))

(use-package yaml-mode)
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc"))

(use-package multiple-cursors
  :if (window-system)
  :bind
  (("C->" . 'mc/mark-next-like-this)
   ("C-<" . 'mc/mark-previous-like-this)))

(use-package anzu
  :if (window-system)
  :config
  (global-anzu-mode 1)
  :bind (("C-M-%" . anzu-query-replace-regexp)
         ("M-%" . anzu-query-replace))
  )

(use-package expand-region
  :bind ("M-#" . er/expand-region))

(use-package ripgrep
  :init (setq wgrep-auto-save-buffer t)
  :bind
  ("C-c M-r" . ripgrep-regexp)
  (:map ripgrep-search-mode-map ("C-x C-q" . ivy-wgrep-change-to-wgrep-mode)))

(use-package deadgrep
  :bind ("C-c C-s C-d" . deadgrep))

(use-package ace-jump-mode
  :disabled
  :init
  (setq ace-jump-mode-case-fold nil)
  :bind
  ("C-c M-c" . ace-jump-mode))

(use-package restclient
  :init
  (add-to-list 'auto-mode-alist '("\\.client$" . restclient-mode))
  :if (window-system))

(use-package company
  :if (window-system)
  :init
  (setq company-tooltip-align-annotations t)
  (setq company-minimum-prefix-length 0)
  (setq company-idle-delay 0.3)
  (setq company-candidates-cache t)
  :config (global-company-mode 1))

(use-package inf-mongo)

(use-package whole-line-or-region
  :init (whole-line-or-region-global-mode 1))

(use-package magit
  :init (setq with-editor-emacsclient-executable nil)
  (defun bob/magit-message (message)
    (interactive "sCommit message: ")
    (magit-commit-create `("-am" ,message)))

  (defun fetch-all-git-repos-in-directory (repos-dir)
    (cl-loop for dir
             in (directory-files repos-dir)
             when (and (file-directory-p (format "%s/%s" repos-dir dir))
                       (member ".git" (directory-files (format "%s/%s" repos-dir dir))))
             do (run-fetch-in-dir (format "%s/%s" repos-dir dir))))

  (defun run-fetch-in-dir (dir)
    (setq default-directory dir)
    (magit-fetch-all-prune))
  :hook
  (before-save-hook . magit-wip-commit-initial-backup)

  :config
  (setq magit-diff-refine-hunk 'all)
  (setq transient-default-level 7)
  (setq magit-commit-show-diff nil
      magit-revert-buffers 1)

  (put 'magit-diff-edit-hunk-commit 'disabled nil)
  (transient-append-suffix 'magit-commit
    "c"
    '("m" "Quick commit using minibuffer for commit message." bob/magit-message))

  (transient-append-suffix 'magit-file-dispatch
    "p"
    '("P" "Push" magit-push))
  (magit-wip-before-change-mode)
  (magit-wip-after-apply-mode)
  (magit-wip-after-save-mode)
  (setq magit-wip-merge-branch t))

(use-package forge
  :init (setq forge-bug-reference-hooks nil))
(use-package magit-todos
  :hook (magit-mode . magit-todos-mode))
(use-package github-review
  :init (setq github-review-fetch-top-level-and-review-comments t))

(use-package git-timemachine)
(use-package diff-hl
  :init (global-diff-hl-mode))

(use-package yasnippet-snippets)
(use-package yasnippet
  :hook
  (prog-mode-hook . yas-minor-mode-on)
  (emacs-lisp-mode . yas-minor-mode-on)
  (js2-mode . yas-minor-mode-on)
  (typescript-mode . yas-minor-mode-on)
  (web-mode . yas-minor-mode-on)
  (text-mode . yas-minor-mode-on)
  (haskell-mode . yas-minor-mode-on)
  :config
  (setq yas-snippet-dirs
        `(,(concat user-emacs-directory "snippets")
          ,yasnippet-snippets-dir))
  (yas-reload-all))

(use-package org-bullets :if (window-system))
(use-package org
  :ensure nil
  :if (window-system)
  :init
  (setq org-loop-over-headlines-in-active-region t)
  (setq calendar-longitude 32.085300)
  (setq calendar-latitude 34.781769)
  (setq org-tree-slide-header nil)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c)")))
  (setq org-directory (concat (getenv "HOME") "/Dropbox/orgzly"))

  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (js . t)
     (shell . t)))
  (add-to-list 'org-src-lang-modes '("tsx" .t ypescript))
  (custom-set-faces
   '(org-agenda-current-time ((t (:inherit org-time-grid :foreground "controlAccentColor")))))
  (require 'ob-js)
  ;; Fix bug in ob-js: https://emacs.stackexchange.com/questions/55690/org-babel-javascript-error
  (setq org-babel-js-function-wrapper
        "console.log(require('util').inspect(function(){\n%s\n}(), { depth: 100 }))")
  :hook
  (org-mode . (lambda () (org-bullets-mode 1)))
  (org-archive . org-save-all-org-buffers)
  (org-after-refile-insert . org-save-all-org-buffers)
  :bind (:map org-mode-map
              ("M-p" . org-metaup)
              ("M-n" . org-metadown)
              ;; ("C-c a" . org-agenda)
              ;; ("C-c c" . org-capture)
              ;; ("C-c S" . org-save-all-org-buffers)
              ("C-c l" . org-store-link)
              ("M-," . org-mark-ring-goto)
              ))

(use-package org-roam
  :init
  (setq org-roam-graph-viewer "/Applications/Brave Browser.app/Contents/MacOS/Brave Browser")
  (setq org-roam-v2-ack t)
  (setq org-roam-directory (concat org-directory "/org-roam"))
  (setq org-id-locations-file (concat org-directory "/.orgids"))
  (setq org-roam-completion-everywhere t)
  (add-to-list 'display-buffer-alist
                  '("\\*org-roam\\*"
                    (display-buffer-in-side-window)
                    (side . right)
                    (slot . 0)
                    (window-width . 0.33)
                    (window-parameters . ((no-other-window . t)
                                          (no-delete-other-windows . t)))))
  :bind
  ;; creates a node if it does not exist, and inserts a link to the node at point:
  ("C-c n i" . 'org-roam-node-insert)
  ("C-c n f" . 'org-roam-node-find)
  ("C-c n c" . 'org-roam-capture)
  ("C-c n b" . org-roam-buffer-toggle)
  :config
  (org-roam-db-autosync-mode))

(use-package shell-defuns :load-path "./site-lisp" :demand t :if (window-system))

(use-package short-lambda :load-path "./site-lisp" :demand t
  :init
  (defun insert-λ ()
    (interactive)
    (insert "λ"))
  :bind
  ("C-x 8 l" . insert-λ))

(use-package vterm
  :if (window-system)
  :after shell-defuns
  :config
  (setq vterm-max-scrollback 100000)
  (define-key vterm-mode-map [remap whole-line-or-region-yank] #'vterm-yank)
  :bind
  ("C-c s s". bob/projectile-run-vterm)
  ("C-c s e" . bob/vterm)
  ("C-c s j" . bob/jump-to-shell)
  (:map vterm-mode-map ("C-c C-j" . vterm-copy-mode))
  (:map vterm-copy-mode-map ("C-c C-j" . vterm-copy-mode)))

(use-package ibuffer
  :ensure nil
  :bind ("C-x C-b" . ibuffer)
  :hook
  (ibuffer-mode . ibuffer-vc-set-filter-groups-by-vc-root)
  :init
  (setq ibuffer-expert t)
  (setq ibuffer-show-empty-filter-groups nil)
  (use-package ibuffer-vc
    :commands (ibuffer-vc-set-filter-groups-by-vc-root)
    :custom
    (ibuffer-vc-skip-if-remote 'nil))
  :custom
  (ibuffer-formats
   '((mark modified read-only locked " "
           (name 35 35 :left :elide)
           " "
           (size 9 -1 :right)
           " "
           (mode 16 16 :left :elide)
           " " filename-and-process)
     (mark " "
           (name 16 -1)
           " " filename))))

(use-package web-mode
  :mode
  ("\\.html\\'" . web-mode)
  ("\\.cssl\\'" . web-mode)
  ("\\.jsx\\'" . web-mode)
  ("\\.vue\\'" . web-mode)
  :init
  (defun lsp-web-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-eslint-apply-all-fixes))
  :hook
  (web-mode . lsp-web-install-save-hooks)
  (web-mode . add-node-modules-path)
  ;; (web-mode . eldoc-mode)
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-attr-indent-offset 18)
  (setq web-mode-enable-auto-pairing nil)
  (setq web-mode-style-padding 2)
  (setq web-mode-indent-style 2)
  (setq web-mode-script-padding 0)
  (setq web-mode-block-padding 2)
  (setq web-mode-comment-style 2)
  (setq web-mode-enable-css-colorization t)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-enable-auto-indentation nil)
  (setq web-mode-enable-auto-expanding t)
  (setq vetur.validation.template t) ;; For lsp-vue
  (setq lsp-vetur-dev-log-level "debug")
  ;; (nvm-use "v12.14.0")
  :bind (:map web-mode-map
              ("C-c C-t C-n" . web-mode-tag-next)
              ("C-c C-t C-p" . web-mode-tag-previous)
              ("C-c C-t C-m" . web-mode-tag-match)
              ("C-c C-t C-e" . web-mode-tag-end)
              ("C-c C-s" . nil)
              ("C-c C-l" . nil)) ;; Unbind insert snippet so deadgrep C-c C-s C-d will work
  )

(use-package tramp
  :ensure nil
  :init (setq tramp-verbose 6)
  :config
  (setq tramp-password-prompt-regexp
        (concat
         "^.*"
         (regexp-opt
          '("passphrase" "Passphrase"
            ;; English
            "password" "Verification code"
            ;; Deutsch
            "passwort" "Passwort"
            ;; Français
            "mot de passe" "Mot de passe")
          t)
         ".*:\0? *"))
  (setq tramp-default-method "ssh")
  (add-to-list 'tramp-restricted-shell-hosts-alist
               "\\bastion\\'")
  (add-to-list 'tramp-default-proxies-alist
               '("bob$" nil "/sshx:bastion:"))
  (setq remote-file-name-inhibit-cache 3600
        tramp-completion-reread-directory-timeout nil
        vc-ignore-dir-regexp (format "%s\\|%s"
                                     vc-ignore-dir-regexp
                                     tramp-file-name-regexp))
  (setq tramp-histfile-override t)
  ;; Save backup files locally
  ;; from https://stackoverflow.com/a/47021266
  (add-to-list 'backup-directory-alist
               (cons tramp-file-name-regexp "/tmp/emacs-backup/")))

(use-package scratch-pop
  :bind ("C-c r" . scratch-pop))

(use-package misc-funcs
  :demand t
  :load-path "./bob-lisp"
  :ensure nil)

;; (use-package undo-fu
;;   :init
;;   (setq undo-fu-allow-undo-in-region t)
;;   :bind
;;   ("C-/" . undo-fu-only-undo)
;;   ("C-?"  . undo-fu-only-redo))

(use-package avy
  :init (setq avy-case-fold-search nil)
  :bind
  ("C-c M-d" . avy-goto-char-in-line)
  ("C-c M-c" . avy-goto-word-1))

(use-package highlight-indent-guides)

(use-package edit-funcs
  :if (window-system)
  :demand t
  :load-path "./bob-lisp"
  :bind
  ("C-`" . unpop-to-mark-command)
  ("M-`" . jump-to-mark))

(use-package json-mode)
(use-package jq-format
  :after json-mode)

(use-package origami
  :bind (:map origami-mode-map
              ("C-=" . origami-toggle-node)))

(use-package ob-mongo :demand :load-path "./ob-mongo" :after org)
(use-package csv-mode)
(use-package ace-window
  :bind ( "C-x o" . ace-window)
  :config
  (setq aw-scope 'frame)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package dockerfile-mode)
(use-package shell-command+
  :bind ("M-!" . shell-command+))

(use-package haskell-mode)
(use-package haskell-snippets)
(use-package cider)
(use-package clojure-mode)
(use-package sicp)

(use-package elfeed
  :init
  (setq elfeed-feeds
        '(
          ;; ("https://www.reddit.com/r/listentothis/.rss" music reddit)
          ("https://www.reddit.com/r/emacs/.rss" programming emacs reddit)
          ("http://notarbut.co/feed/podcast" podcast)
          ("https://blog.rust-lang.org/feed.xml" programming rust)
          ;; ("https://www.reddit.com/r/rust/.rss" programming rust reddit)
          ;; ("https://www.reddit.com/r/Clojure/.rss" programming clojure reddit)
          ("https://feed.podbean.com/geekonomy/feed.xml" podcast)
          ))
  :bind ("C-c w" . elfeed))

(put 'dired-find-alternate-file 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
