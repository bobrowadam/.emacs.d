(setq gc-cons-threshold 100000000)
(setq debug-on-error nil)
(setq package-enable-at-startup nil)
(require 'cl-lib)

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(global-hl-line-mode nil)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(setq Info-additional-directory-list `(,(expand-file-name "info-docs" user-emacs-directory)))
(unless (not (file-exists-p custom-file))
  (load custom-file))

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'el-patch)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq package-native-compile nil)
(setq use-package-always-defer t)
(setq use-package-always-ensure t)
(setq use-package-verbose t)
(setq use-package-debug t)
(setq use-package-compute-statistics t)

(require 'server)
(unless (server-running-p)
  (server-start))
(use-package winner-mode
  :ensure nil
  :init (winner-mode 1))

(use-package exec-path-from-shell
  :if (window-system)
  :init
  (setq exec-path-from-shell-arguments nil)
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
      dired-recursive-deletes 'always
      dired-recursive-copies 'always)

;; (setq initial-major-mode 'org-mode)
(setq initial-scratch-message ";; Oh it's you again")

;; Vertical Scroll
(pixel-scroll-precision-mode t)
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

(set-default 'cursor-type  '(bar . 3))
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

(global-unset-key (kbd "s-n"))

;; Deal with editing large files:
(global-so-long-mode 1)

;; Theme and Font
(setq custom-safe-themes t)
(setq custom-theme-directory "~/.emacs.d/themes")
(set-frame-font "DaddyTimeMono Nerd Font 21")
;; (set-frame-font "SauceCodePro Nerd Font 21")
;; (set-frame-font "Roboto Mono 21")
(add-to-list 'default-frame-alist
             '(font . "DaddyTimeMono Nerd Font 21"))

(use-package doom-modeline
  :demand t
  :custom
  ;; Don't compact font caches during GC. Windows Laggy Issue
  (inhibit-compacting-font-caches t)
  (doom-modeline-minor-modes nil)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-height 15)
  ;; (doom-modeline-lsp t)
  (find-file-visit-truename t)
  (doom-modeline-github t)
  :config
  (doom-modeline-mode))

(use-package immaterial-theme)
(use-package doom-themes
  :disabled t
  :if (window-system)
  :demand t
  :config
  ;; (load-theme 'bob-doom-homage-black t)
  ;; (load-theme 'doom-ayu-mirage t)
  ;; (load-theme 'doom-monokai-spectrum t)
  (load-theme 'doom-old-hope t)
  ;; (load-theme 'doom-homage-black)
  ;; (load-theme 'doom-oceanic-next t)
  ;; (load-theme 'doom-acario-dark t)
  ;; (load-theme 'doom-Iosvkem t)
  ;; (load-theme 'doom-moonlight t)
  ;; (load-theme 'bobs-badger t)
  ;; (load-theme 'naysayer)
  ;; (load-theme 'modus-vivendi)
  ;; (load-theme 'doom-gruvbox)
  ;; (load-theme 'doom-ir-black)
  ;; (load-theme 'doom-sourcerer)
  (setq doom-themes-treemacs-theme "doom-colors"))

(use-package modus-themes
  :demand t
  :custom
  (modus-themes-mode-line '(borderless padded accented))
  (modus-themes-region '( bg-only))
  (modus-themes-completions
   (quote ((t . (extrabold intense background)))))
  (modus-themes-bold-constructs t)
  (modus-themes-italic-constructs t)
  (modus-themes-paren-math ('bold intense))
  (modus-themes-hl-line nil)
  (modus-themes-syntax '(alt-syntax yellow-comments green-strings))
  :config
  (load-theme 'modus-vivendi))

(use-package gruvbox-theme)

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

;; Completions
(use-package consult
  :demand t
  :after projectile
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c m" . consult-mode-command)
         ("C-x r b" . consult-bookmark)
         ("C-c k" . consult-kmacro)
         ("C-x M-:" . consult-complex-command)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)
         ("C-M-#" . consult-register)
         ("M-y" . consult-yank-pop)
         ("<help> a" . consult-apropos)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-i" . consult-imenu)
         ("M-g i" . consult-imenu-multi)
         ("C-c M-s f" . consult-find)
         ("C-c M-s F" . consult-locate)
         ("C-c M-s G" . consult-git-grep)
         ("C-c C-s C-r" . consult-ripgrep)
         ("C-M-s" . consult-line)
         ("C-c M-s m" . consult-multi-occur)
         ("C-c M-s k" . consult-keep-lines)
         ("C-c M-s e" . consult-isearch)
         :map isearch-mode-map
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi))
  :init
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  ;; (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  ;; :config
  ;; (consult-customize
  ;;  consult-theme
  ;;  :preview-key '(:debounce 0.2 any)
  ;;  consult-ripgrep consult-git-grep consult-grep
  ;;  consult-bookmark consult-recent-file consult-xref
  ;;  consult--source-project-file consult--source-bookmark
  ;;  :preview-key (kbd "M-."))

  (setq consult-narrow-key "<")
  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-roots project)))))
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-root-function #'projectile-project-root)
  (projectile-load-known-projects)
  (setq my-consult-source-projectile-projects
        `(:name "Projectile projects"
                :narrow   ?P
                :category project
                :action   ,#'projectile-switch-project-by-name
                :items    ,projectile-known-projects))
  ;; (add-to-list 'consult-buffer-sources my-consult-source-projectile-projects 'append)
)
(use-package consult-lsp :ensure t)

(use-package vertico
  :disabled t
  :init
  (vertico-mode)
  ;; Grow and shrink the Vertico minibuffer
  (setq vertico-resize t)
  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t))

(use-package orderless
  :disabled t
  :init
  (setq completion-styles '(substring orderless)
        orderless-skip-highlighting (lambda () selectrum-is-active)
        completion-category-defaults nil
        completion-ignore-case t
        completion-category-overrides '((file (styles partial-completion)))))

(use-package hotfuzz
  :after selectrum
  :ensure t
  :custom
  (completion-ignore-case t)
  :init
  (hotfuzz-selectrum-mode))

(use-package savehist-mode
  :ensure nil
  :init
  (savehist-mode))

(use-package recentf-mode
  :ensure nil
  :init
  (recentf-mode 1))

(use-package selectrum
  :bind (("C-M-r" . selectrum-repeat)
         :map selectrum-minibuffer-map
         ("C-r" . selectrum-select-from-history)
         :map minibuffer-local-map
         ("M-h" . backward-kill-word))
  :custom
  (selectrum-fix-minibuffer-height t)
  (selectrum-num-candidates-displayed 7)
  ;; (selectrum-refine-candidates-function #'orderless-filter)
  ;; (selectrum-highlight-candidates-function #'orderless-highlight-matches)
  :custom-face
  (selectrum-current-candidate ((t (:background "#3a3f5a"))))
  :init
  (selectrum-mode 1))

(use-package marginalia
  ;; :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)
   ;; ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings))

  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))


(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; Dired
(use-package dired
  :config
  (setq dired-use-ls-dired nil)
  (setq dired-listing-switches "-alh")
  :hook (dired-mode . (lambda () (dired-hide-details-mode 1)))
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

(defun npm-run-build ()
  "Build typescript project on watch mode"
  (interactive)
  (when (and (projectile-project-name) (eq major-mode 'typescript-mode))
   (async-shell-command "npm run build -- -w" (format "%s: TS-COMPILE" (projectile-project-name)))))
(use-package typescript-mode
  :init
  (defun lsp-ts-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-eslint-apply-all-fixes))
  :hook
  (typescript-mode . lsp-ts-install-save-hooks)
  (typescript-mode . add-node-modules-path)
  (typescript-mode . origami-mode)
  :bind (:map typescript-mode-map ("C-c C-b" . npm-run-build))
  :config
  (setq typescript-indent-level 2))

(use-package jest-test-mode
  :commands jest-test-mode
  :hook (typescript-mode js-mode typescript-tsx-mode))

(use-package js2-mode
  :init
  (defun lsp-js-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-eslint-apply-all-fixes))
  :init
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
  :disabled t
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
  (dap-register-debug-template
   "ts-node:launch"
   (list :name "TS Index"
         :type "node"
         :request "launch"
         :args ["${workspaceFolder}/node_modules/env-setter/src/ssm-entrypoint-local.js" "${workspaceFolder}/costa.ts"]
         :runtimeArgs ["-r" "ts-node/register"]
         :sourceMaps t
         :cwd "${workspaceFolder}"
         :protocol "inspector"))
  (require 'dap-lldb)
  (require 'dap-gdb-lldb)
  ;; installs .extension/vscode
  (dap-gdb-lldb-setup)
  (dap-register-debug-template
   "Rust::LLDB Run Configuration"
   (list :type "lldb"
         :request "launch"
         :name "LLDB::Run"
         :gdbpath "rust-lldb"
         :target nil
         :cwd nil))
  :bind
  (:map dap-mode-map
        ("C-c d" . dap-hydra)))

(use-package dap-ui
  :disabled t
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

(use-package nodejs-repl)
(use-package nvm)

(use-package eglot
  :disabled t
  :config
  (add-to-list 'eglot-server-programs '(web-mode "vls"))
  ;; (add-hook web-mode-hook #'eglot-ensure)
  :hook ((js2-mode typescript-mode web-mode
                   c-mode c++-mode rust-mode
                   svelte-mode
                   ;; haskell-mode
                   ) . eglot-ensure))

(use-package lsp-mode
  :commands lsp
  :init
  (advice-add 'lsp :before (lambda (&rest _args) (eval '(setf (lsp-session-server-id->folders (lsp-session)) (ht)))))
  :custom
  (lsp-prefer-flymake nil)           ; Use flycheck instead of flymake
  (lsp-file-watch-threshold 2000)
  (lsp-eslint-auto-fix-on-save t)
  (read-process-output-max (* 1024 1024))
  ;; (lsp-eldoc-hook nil)
  (lsp-ui-doc-show-with-cursor t)
  (company-lsp-cache-candidates t)
  (fnm-node "/Users/bob/Library/Application\ Support/fnm/node-versions/v18.5.0/installation/bin/node")
  (lsp-eslint-server-command `(,fnm-node
                               ,(f-join lsp-eslint-unzipped-path "extension/server/out/eslintServer.js")
                               "--stdio"))
  (lsp-eslint-node fnm-node)
  :bind
  (:map lsp-mode-map
        ("C-c C-f" . lsp-format-buffer)
        ("C-c C-n" . lsp-rename)
        ("C-c C-r" . lsp-ui-peek-find-references)
        ("M-." . lsp-find-definition)
        ("M-n" . forward-paragraph)
        ("M-p" . backward-paragraph))
  :hook ((js2-mode typescript-mode web-mode
                   c-mode c++-mode rust-mode
                   svelte-mode
                   ;; haskell-mode
                   ) . lsp))

(use-package lsp-ui
  :disabled t
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

  :custom
  (lsp-ui-sideline-show-hover nil)
  ;; (lsp-ui-doc-position 'bottom)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-border (face-foreground 'default))
  (lsp-ui-sideline-enable nil)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-code-actions nil)
  :config
  ;; Use lsp-ui-doc-webkit only in GUI
  (if (display-graphic-p)
      (setq lsp-ui-doc-use-webkit t))
)

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

(use-package perspective
  :after consult

  (push consult--source-perspective consult-buffer-sources)
  :custom
  (persp-initial-frame-name "Main")
  (persp-state-default-file (concat user-emacs-directory ".persp-state"))
  :bind ("C-x k" . kill-this-buffer)
  :config
  (defvar consult--source-perspective
    `(:name     "Perspective"
                :narrow   (?s . "Perspective")
                :hidden   t
                :category buffer
                :face     consult-buffer
                :history  buffer-name-history
                :state    ,#'consult--buffer-state
                :enabled  ,(lambda () consult-project-root-function)
                :items
                ,(lambda ()
                    (consult--buffer-query :sort 'visibility
                                           :include (persp-get-buffer-names)
                                           :as #'buffer-name)))
    "Project buffer candidate source for `consult-buffer'.")
  ;; Running `persp-mode' multiple times resets the perspective list...
  (unless (equal persp-mode t)
    (persp-mode)))

(use-package projectile
  :init
  (projectile-mode 1)
  :config
  (add-to-list 'projectile-globally-ignored-directories "node_modules")
  (setq projectile-switch-project-action #'projectile-dired)
  (setq projectile-enable-caching nil)
  (setq projectile-sort-order 'recentf)
  ;; (setq projectile-sort-order 'recently-active)
  ;; (projectile-global-mode 1)
  (unless projectile-known-projects
    (-let ((main-projects-directory (read-directory-name "Please enter main projects directory")))
      (projectile-discover-projects-in-directory main-projects-directory)))
  :bind
  (:map projectile-mode-map ("C-c p" . projectile-command-map)))

(use-package paredit
  :hook
  (eval-expression-minibuffer-setup . enable-paredit-mode)
  (emacs-lisp-mode . enable-paredit-mode)
  ;; (clojure-mode . enable-paredit-mode)
  ;; (cider-mode . enable-paredit-mode)
  (slime-mode . enable-paredit-mode)
  (slime-repl-mode . enable-paredit-mode)
  (common-lisp-mode . enable-paredit-mode)
  (lisp-mode . enable-paredit-mode)
  :bind
  (:map paredit-mode-map ("C-'" . sp-rewrap-sexp)))

(use-package smartparens
  :demand t
  :init
  (setq sp-ignore-modes-list
        '(minibuffer-inactive-mode emacs-lisp-mode eval-expression-minibuffer-setup common-lisp-mode lisp-mode sly-mode))
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
  :disabled t
  :if (window-system)
  :bind
  (("C->" . 'mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("C-M-," . mc/mark-pop)))

(use-package anzu
  :demand t
  :if (window-system)
  :config
  (global-anzu-mode 1)
  :bind (("C-M-%" . anzu-query-replace-regexp)
         ("M-%" . anzu-query-replace))
  )

(use-package expand-region
  :bind ("M-#" . er/expand-region))

(use-package rg
  :bind
  ("C-c M-r" . rg))

(use-package wgrep)

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
  (global-company-mode 1))

(use-package inf-mongo
   :after org)

(use-package whole-line-or-region
  :disabled t
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
  :bind  ("C-c s g" . bob/magit-buffers)
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
  (transient-append-suffix 'magit-file-dispatch
    "P"
    '("F" "Pull" magit-pull))
  (magit-wip-before-change-mode)
  (magit-wip-after-apply-mode)
  (magit-wip-after-save-mode)
  (setq magit-wip-merge-branch t))

(use-package forge
  :init (setq forge-bug-reference-hooks nil))

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
  (lisp-mode . yas-minor-mode-on)
  ;; (haskell-mode . yas-minor-mode-on)
  (rust-mode . yas-minor-mode-on)
  :config
  (setq yas-snippet-dirs
        `(,(concat user-emacs-directory "snippets")
          ,yasnippet-snippets-dir))
  (yas-reload-all))

(use-package common-lisp-snippets)

(use-package org-bullets
  :disabled t
  :if (window-system))

(defvar tb/org-todo-bullet-faces
    '(("NEXT" . (:inherit base-todo-keyword-face :foreground "#FF8580"))
      ("TODO" . (:inherit base-todo-keyword-face :foreground "#CDCD85853F3F"))
      ("ISSUE" . (:inherit base-todo-keyword-face :foreground "#FF8580"
                           :family "github-octicons" :height 160))
      ("BRANCH" . (:inherit base-todo-keyword-face :foreground "#D58422"
                            :family "github-octicons"))
      ("FORK" . (:inherit base-todo-keyword-face :foreground "#D58422"
                            :family "github-octicons"))
      ("MR" . (:inherit base-todo-keyword-face :foreground "#C7A941"
                        :family "github-octicons"))
      ("MERGED" . (:inherit base-todo-keyword-face :foreground "#75AD18"
                            :family "github-octicons"))
      ("GITHUB" . (:inherit base-todo-keyword-face :foreground "#BBBBBB"
                            :family "github-octicons" :height 160))
      ("DONE" . (:inherit base-todo-keyword-face :foreground "#75AD18"))
      ("IDEA" . (:inherit base-todo-keyword-face :foreground "#85AAFF"))
      ("WRITE" . (:inherit base-todo-keyword-face :foreground "#FF8580"))
      ("WRITING" . (:inherit base-todo-keyword-face :foreground "#C7A941"))
          ))

(use-package org-superstar
  :straight '(org-superstar
              :fork (:host github
                           :repo "thibautbenjamin/org-superstar-mode"))
  :after org
  :hook (org-mode . org-superstar-mode)
  :config
  (set-face-attribute 'org-superstar-header-bullet nil :inherit 'fixed-pitched :height 180)
  :custom
  ;; set the leading bullet to be a space. For alignment purposes I use an em-quad space (U+2001)
  ;; (org-superstar-headline-bullets-list '(" "))
  (org-superstar-todo-bullet-alist '(("DONE" . ?✔)
                                     ("NEXT" . ?☞)
                                     ("TODO" . ?⌖)
                                     ("ISSUE" . ?)
                                     ("BRANCH" . ?)
                                     ("FORK" . ?)
                                     ("MR" . ?)
                                     ("MERGED" . ?)
                                     ("GITHUB" . ?A)
                                     ("WRITING" . ?✍)
                                     ("WRITE" . ?✍)
                                     ))
  (org-superstar-special-todo-items t)
  (org-superstar-leading-bullet " ")
  (org-superstar-todo-bullet-face-alist tb/org-todo-bullet-faces))

(use-package org
  :demand t
  ;; :defer 5
  :ensure nil
  :if (window-system)
  :init
  (setq org-export-with-toc nil)
  (setq org-pretty-entities t)
  (setq org-loop-over-headlines-in-active-region t)
  (setq calendar-longitude 32.085300)
  (setq calendar-latitude 34.781769)
  (setq org-tree-slide-header nil)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c)")))
  (setq org-stuck-projects
        '("+LEVEL=1+PROJECT" ("NEXT" "WAITING") ("@IGNORE" "@REMINDER") ""))
  ;; +LEVEL=3+boss-TODO​="DONE"
  ;; (setq org-tags-exclude-from-inheritance '("PROJECT"))
  (setq org-tags-exclude-from-inheritance nil)
  (setq org-directory (concat (getenv "HOME") "/Dropbox/orgzly"))
  (setq org-capture-templates
        `(("t" "entry" entry (file ,(concat org-directory "/org-roam/20211126120714-inbox.org")) "* %?\n  %i")))
  (setq org-agenda-files
        (list
          ;; ,(concat org-directory "/riseup-google-calendar.org")
          ;; ,(concat org-directory "/private-google-calendar.org")
          (concat org-directory "/org-roam/20211126120714-inbox.org")
          (concat org-directory "/org-roam/20211126182152-tasks.org")
          (concat org-directory "/org-roam/20211126120120-projects.org")
          (concat org-directory "/org-roam/20211126112747-check_this_up.org")
          (concat org-directory "/org-roam/20211126120630-sometime.org")
          (concat org-directory "/org-roam/20211208225633-reminders.org")))
  (setq org-deadline-warning-days 3)
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
  (setenv "NODE_PATH" "/Users/bob/.nvm/versions/node/v16.13.1/bin/node")
  ;; (setenv "NODE_PATH" "/Users/bob/.nvm/versions/node/v12.14.0/lib/node_modules")
  ;; Fix bug in ob-js: https://emacs.stackexchange.com/questions/55690/org-babel-javascript-error
  ;; (setq org-babel-js-function-wrapper
  ;;       "console.log(require('util').inspect(function(){\n%s\n}(), { depth: 100 }))")
  :hook
  (org-mode . (lambda () (org-superstar-mode 1)))
  (org-archive . org-save-all-org-buffers)
  (org-after-refile-insert . org-save-all-org-buffers)
  :bind
  (:map org-mode-map
        ("M-p" . org-metaup)
        ("M-n" . org-metadown)
        ;; ("C-c c" . org-capture)
        ;; ("C-c S" . org-save-all-org-buffers)
        ("C-c l" . org-store-link)
        ("M-," . org-mark-ring-goto)
        ("M-F" . org-shiftright)
        ("M-B" . org-shiftleft)
        ("C-c n r" . org-roam-refile))
  (:map org-read-date-minibuffer-local-map
        ("M-f" . (lambda () (interactive (org-eval-in-calendar '(calendar-forward-day 1)))))
        ("M-b" . (lambda () (interactive (org-eval-in-calendar '(calendar-backward-day 1)))))
        ("M-p" . (lambda () (interactive (org-eval-in-calendar '(calendar-backward-week 1)))))
        ("M-n" . (lambda () (interactive (org-eval-in-calendar '(calendar-forward-week 1)))))))

(use-package org-agenda
  :after org
  :bind
  ("C-c a" . org-agenda)
  ("C-c c" . org-capture)
  (:map org-agenda-mode-map
        ("M-F" . org-agenda-do-date-later)
        ("M-B" . org-agenda-do-date-earlier))
  :ensure nil)

(use-package org-roam
  :init
  (setq org-roam-graph-viewer "/Applications/Brave Browser.app/Contents/MacOS/Brave Browser")
  (setq org-roam-v2-ack t)
  (setq org-roam-directory (concat org-directory "/org-roam"))
  (setq org-id-locations-file (concat org-directory "/.orgids"))
  (setq org-roam-completion-everywhere t)
  (setq org-roam-dailies-directory "journal/")
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
  ("C-c n i" . org-roam-node-insert)
  ("C-c n f" . org-roam-node-find)
  ("C-c n c" . org-roam-capture)
  ("C-c n b" . org-roam-buffer-toggle)
  ("C-c n d d" . org-roam-dailies-capture-today)
  :config
  (org-roam-db-autosync-mode))

(use-package org-roam-bibtex
  :after org-roam
  :config
  (require 'org-ref))

(use-package org-roam-ui
  :after org-roam
  ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;;         a hookable mode anymore, you're advised to pick something yourself
  ;;         if you don't care about startup time, use
  ;;  :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

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

(defun ibuffer-filter-by-prog-mode  ()
  (ibuffer-filter-by-derived-mode 'prog-mode))

(defun short--file-path (file-path)
  (s-prepend "/" (s-join "/" (-take-last 4 (s-split "/" file-path)))))

(use-package ibuffer
  :demand
  :ensure nil
  :bind ("C-x C-b" . ibuffer)
  :init
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
               (unless (eq ibuffer-sorting-mode 'recency)
                 (ibuffer-do-sort-by-recency)))))

(use-package all-the-icons-ibuffer
  :disabled t
  :init (all-the-icons-ibuffer-mode 1))

(use-package ibuffer-vc
  :demand
  :commands (ibuffer-vc-set-filter-groups-by-vc-root)
  :custom
  (ibuffer-vc-skip-if-remote t))

(use-package web-mode
  :mode
  ("\\.html\\'" . web-mode)
  ("\\.cssl\\'" . web-mode)
  ("\\.jsx\\'" . web-mode)
  ("\\.vue\\'" . web-mode)
  ;; ("\\.svelte\\'" . web-mode)
  :init
  (defun lsp-web-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-eslint-apply-all-fixes))
  :hook
  (web-mode . lsp-web-install-save-hooks)
  (web-mode . add-node-modules-path)
  (web-mode . eldoc-mode)
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

(use-package svelte-mode
  :disabled t
  :bind
  (:map svelte-mode-map
        ("C-M-f" . sgml-skip-tag-forward)
        ("C-M-b" . sgml-skip-tag-backward)))

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
               (cons tramp-file-name-regexp "/tmp/emacs-backup/"))
)

(use-package scratch-pop
  :bind ("C-c r" . scratch-pop))

(use-package misc-funcs
  :demand t
  :load-path "./bob-lisp"
  :ensure nil)

(use-package undo-fu
  :disabled t
  :init
  (setq undo-fu-allow-undo-in-region t)
  :bind
  ("C-/" . undo-fu-only-undo)
  ("C-?"  . undo-fu-only-redo))

(use-package avy
  :init (setq avy-case-fold-search nil)
  :bind
  ("C-c M-d" . avy-goto-char-in-line)
  ("C-c M-c" . avy-goto-word-1))

(use-package highlight-indent-guides
  :init
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-responsive 'top)
  :hook (prog-mode . highlight-indent-guides-mode))

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

(use-package ob-mongo  :demand t :load-path "./ob-mongo" :after org)
(use-package csv-mode)
(use-package ace-window
  :bind ( "C-x o" . ace-window)
  :config
  (setq aw-scope 'frame)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package dockerfile-mode)
(use-package shell-command+
  :bind ("M-!" . shell-command+))

(use-package haskell-mode :disabled t)
(use-package haskell-snippets :disabled t)
(use-package cider :disabled t)
(use-package clojure-mode :disabled t)
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
          ("https://danluu.com/atom.xml" programming blog)
          ("https://feed.podbean.com/geekonomy/feed.xml" podcast)
          ("https://protesilaos.com/master.xml" programming blog)
          ))
  :bind ("C-c w" . elfeed))

(use-package slime
  :disabled t
  :config
  (setq inferior-lisp-program "/usr/local/bin/sbcl")
  (slime-setup '(slime-fancy slime-company)))

(use-package slime-company
  :disabled t
  :after slime)

(use-package sly
  :init
  (setq inferior-lisp-program "/usr/local/bin/sbcl")
  :hook
  (sly-mode . (lambda ()
     (unless (sly-connected-p)
       (save-excursion (sly))))))

(use-package sly-asdf)
(use-package sly-quicklisp)

(use-package racket-mode :disabled t)

(use-package racket-xp
  :disabled t
  :after (racket-mode)
  :ensure nil
  :hook
  (racket-mode . (lambda () (racket-xp-mode 1))))

(use-package docker)
(use-package docker-tramp)
(use-package flyspell
  :bind  (:map flyspell-mode-map ("C-;" . nil))
  :ensure nil
  :hook
  (prog-mode . flyspell-prog-mode)
  (org-mode . flyspell-mode)
  (git-commit-setup . git-commit-turn-on-flyspell)

  :config
  (setq flyspell-issue-message-flag nil))

;; Enable sbt mode for executing sbt commands
(use-package sbt-mode
  :disabled t
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
   ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
   (setq sbt:program-options '("-Dsbt.supershell=false")))

(use-package darkroom
  :commands darkroom-mode
  :config
  (setq darkroom-text-scale-increase 0))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package zoom-window :bind ("C-x C-z" . zoom-window-zoom))
(use-package ox-gfm :after org)
(use-package emojify)

(use-package dirvish
  :disabled t
  :init
  (dirvish-override-dired-mode))

(use-package tree-sitter
  :ensure t
  :config
  ;; activate tree-sitter on any buffer containing code for which it has a parser available
  (global-tree-sitter-mode)
  ;; you can easily see the difference tree-sitter-hl-mode makes for python, ts or tsx
  ;; by switching on and off
  :hook (tree-sitter-after-on-hook .  #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :ensure t
  :demand t
  :after tree-sitter)

(use-package tsi
  :demand t
  :after tree-sitter
  :straight '(tsi :type git :host github :repo "orzechowskid/tsi.el")
  :commands (tsi-typescript-mode tsi-json-mode tsi-css-mode)
  :hook
  (typescript-mode . tsi-typescript-mode)
  (json-mode . tsi-typescript-mode)
  (css-mode . tsi-typescript-mode)
  (scss-mode . tsi-typescript-mode))

(use-package pdf-tools)
(use-package hcl-mode
  :mode
  ("\\.dsl\\'" . hcl-mode))

(use-package iedit
  :bind ("C-;" . iedit-mode))

(use-package grammarly)
(use-package flycheck-grammarly)
(use-package prettier)
(use-package pandoc-mode)

(put 'dired-find-alternate-file 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'magit-clean 'disabled nil)
