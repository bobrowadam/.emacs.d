;; -*- lexical-binding: t; -*-

(use-package flymake
  :custom
  (flymake-error-bitmap '(right-arrow modus-themes-prominent-error)))

(use-package fancy-compilation
  :config
  (setq fancy-compilation-term "xterm-256color")
  :commands (fancy-compilation-mode))

(with-eval-after-load 'compile
  (fancy-compilation-mode))

(defun is-typescript-project ()
  "Returns true when project has an NPM build script for typescript"
  (if-let* ((project (project-current))
            (default-directory (project-root project))
            (package-json-raw (read-file "package.json"))
            (package-json (json-parse-string package-json-raw
                                             :object-type 'alist)))
      (s-matches-p (assocdr 'check-types (assocdr 'scripts package-json))
               "tsc --noEmit -p tsconfig.app.json")))

(defun npm-run-build ()
  "Build typescript project on watch mode"
  (interactive)
  (when (is-typescript-project)
    (let ((default-directory (project-root (project-current t)))
          (comint-scroll-to-bottom-on-input t)
          (comint-scroll-to-bottom-on-output t)
          (comint-process-echoes t)
          (compilation-buffer-name (format "TS-COMPILE -- %s"
                                           (get-dir-name (nth 2 (project-current))))))
      (cond ((and (not (eq major-mode 'comint-mode))
                  (car (memq (get-buffer compilation-buffer-name)
                             (buffer-list))))
             (switch-to-buffer (get-buffer compilation-buffer-name)))
            ((and (eq major-mode 'comint-mode)
                  (s-contains? (buffer-name (current-buffer)) compilation-buffer-name))
             (switch-to-prev-buffer))
            (t
             (compilation-start (bob/generate--run-service-command (project-name (project-current)))
                                t (lambda (_)
                                    compilation-buffer-name)))))))

(defun get--available-inspect-port ()
  (if-let (inspect-processes (get--inspect-processes-port))
      (1+ (car (-sort '> inspect-processes)))
    9229))

(defun get--inspect-processes-port ()
  (cl-remove-if-not 'identity
                    (mapcar
                     (lambda (process)
                       (if-let ((match (s-match "inspect=\\([0-9]+\\)" (nth 2 (process-command process)))))
                           (string-to-number (cadr match))))
                     (cl-remove-if-not
                      (lambda (p) (s-contains? "comint" (process-name p)))
                      (process-list)))))

(defun bob/compilation-buffer-name ()
  (if-let ((projcet-path (nth 2 (project-current))))
      (format "TS-COMPILE -- %s"
              (get-dir-name projcet-path))))

(defun npm-run (&optional normal-mode)
  "Debug typescript project on watch mode
NORMAL-MODE is for not running with debugger"
  (interactive "P")
  (when (is-typescript-project)
    (let ((default-directory (project-root (project-current t)))
          (comint-scroll-to-bottom-on-input t)
          (comint-scroll-to-bottom-on-output t)
          (comint-process-echoes t)
          (compilation-buffer-name (bob/compilation-buffer-name))
          (project-main-file (bob/npm--project-name)))
      (cond ((and (not (eq major-mode 'comint-mode))
                  (car (memq (get-buffer compilation-buffer-name)
                             (buffer-list))))
             (switch-to-buffer (get-buffer compilation-buffer-name)))
            ((and (eq major-mode 'comint-mode)
                  (s-contains? (buffer-name (current-buffer)) compilation-buffer-name))
             (switch-to-prev-buffer))
            (t
             (let ((compilation-command (if normal-mode
                                            (format "./node_modules/typescript/bin/tsc -w& nodemon -d 2 -w ./dist -r source-map-support/register ./dist/%s.js"
                                                    project-main-file)
                                          (format "./node_modules/typescript/bin/tsc -w& nodemon -d 2 --inspect=%s -w ./dist -r source-map-support/register ./dist/%s.js"
                                                  (get--available-inspect-port)
                                                  project-main-file))))
               (with-temporary-node-version
                   (fnm-current-node-version)
                 (compilation-start compilation-command
                                    t (lambda (mode)
                                        compilation-buffer-name)))))))))

(defun bob/npm--project-name ()
  "Get the current project name from the package json file."
  (when-let ((project-root-path (project-root (project-current)))
             (package-json (json-parse-string (read-file (format "%s/package.json" project-root-path))
                                              :object-type 'alist)))
    (assocdr 'name package-json)))

(defun bob/project--main-file-overrides (project-name)
  "Helper function for `npm-run`.
If the main file of an NPM project is not "
  (pcase  project-name
    ("highland-workshop" "highlander")
    ("skeleton-typescript-service" "example")
    (_ project-name)))

(defun bob/get-inspect-port ()
  (if-let ((compilation-process (get-buffer-process (bob/compilation-buffer-name)))
           (inspect-string (--find
                            (s-contains? "inspect" it)
                            (split-string (assocdr
                                           'args
                                           (process-attributes (process-id compilation-process)))
                                          "\s"))))
      (string-to-number (cadr (split-string inspect-string "=")))
    9229))

(defun npm-install-project (&optional force quiet)
  "NPM install in project.
If FORCE is non-nil, delete the \"package-lock.json\" and \"node_modules\"
directories and verify NPM cache before running `npm install`."
  (interactive "P")
  (when (read-file "package.json")
   (let* ((default-directory (project-root (project-current t)))
          (buffer-name (format "*npm install %s*" default-directory)))
     (message "local NPM executable version is %s" (s-trim-right (shell-command-to-string "npm -v")))
     (when force
       (message "removing package-lock.json")
       (unwind-protect (delete-file (concat default-directory "package-lock.json")))
       (message "removing node_modules")
       (unwind-protect (delete-directory (concat default-directory "node_modules") t))
       (message "verifying NPM's cache")
       (apply #'call-process "node" nil 0 nil '("verify")))
     (with-temporary-node-version (fnm-current-node-version)
       (if (not quiet)
           (compilation-start "npm i")
         (make-process :name buffer-name
                       :buffer buffer-name
                       :command '("npm" "i" "--no-color")
                       :sentinel (process-generic-sentinel)))))))

(defun bob/update-node-modules-if-needed (&optional root)
  "Check if node_modules should be updated by using \"npm list\"."
  (interactive "P")
  (when-let* ((default-directory (or root (and (project-current)
                                               (project-root (project-current)))))
              (is-node-project (read-file "package.json"))
              (buffer-name (format "*npm-list*: %s %s" default-directory (random 100000))))
    (with-temporary-node-version (fnm-current-node-version)
      (make-process :name buffer-name
                    :buffer buffer-name
                    :command '("npm" "list" "--no-color" "--depth=0")
                    :sentinel (bob/npm-outdated-sentinel (current-time))))))

(defun bob/npm-list-problems-p (npm-list-results)
  (seq-remove
   (lambda (str) (s-starts-with? "npm ERR! peer dep missing" str))
   (seq-filter (lambda (str)
                 (s-starts-with? "npm ERR!" str))
               (split-string npm-list-results "\n"))))

(defun bob/npm-outdated-sentinel (start-time)
  "Sentinel for handling npm outdated process termination."
  (lambda (process event)
    (with-current-buffer (process-buffer process)
      (message (format "bob/npm-outdated-sentinel eval time is %.06f"
                       (float-time (time-since start-time))))
      (when (and (eq (process-exit-status process) 1)
                 (bob/npm-list-problems-p (buffer-string))
                 (y-or-n-p "node_modules are *not* up to date, Do you want to run `\"npm install\"")
                 (npm-install-project nil t)))
      (kill-buffer (current-buffer)))))

(use-package typescript-mode
  :ensure nil
  :bind
  ("C-c C-b" . npm-run-build)
  ("C-c C-r" . npm-run)
  :mode ("\\.ts$" . typescript-ts-mode)
  :mode ("\\.tsx$" . tsx-ts-mode)
  :config
  (setq-default typescript-indent-level 2))

;; (use-package tsx-ts-mode
;;   :ensure nil
;;   :mode ("\\.tsx$" . tsx-ts-mode)
;;   :config
;;   (setq-default typescript-indent-level 2))

(use-package jest-test-mode
  :init
  :commands jest-test-mode
  :straight (jest-test-mode :type git :host github :repo "rymndhng/jest-test-mode")
  ;; :custom
  ;; (jest-test-command-string (format "%s %%s ./node_modules/.bin/jest %%s %%s" "node"))
  ;; (jest-test-command-string (format "node %%s ./node_modules/jest/bin/jest %%s %%s"))
  :hook (typescript-mode typescript-ts-mode js-mode))

(defun jest-set-config-file ()
  (interactive)
  "Ask the user to choose a jest config file from the project root"
  (if-let ((config-file (completing-read "Choose a jest config file: "
                                         (directory-files (project-root (project-current))
                                                          nil
                                                          "jest"))))
      (add-to-list 'jest-test-options (format "-c=%s" config-file))))

(use-package js-ts-mode
  :ensure nil
  )
(use-package js2-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  :hook
  (js2-mode . js2-imenu-extras-mode)
  (js2-mode . js2-mode-hide-warnings-and-errors)
  (js2-mode . electric-indent-mode)
  :bind (:map js2-mode-map
              ("C-<tab>" . js2-indent-bounce)
              ("C-c C-s" . nil)
              ("C-x C-e" . js-send-last-sexp))
  :config
  (setq-default js2-auto-indent-p nil)
  (setq-default unset-electric-indent)
  (setq-default js-indent-level 2))

(use-package web-mode
  :mode
  ("\\.html\\'" . web-mode)
  ("\\.cssl\\'" . web-mode)
  ;; ("\\.jsx\\'" . web-mode)
  ;; ("\\.tsx\\'" . web-mode)
  ;; ("\\.vue\\'" . web-mode)
  :hook
  (web-mode . (lambda () (setq-local font-lock-defaults nil)))
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
  :bind (:map web-mode-map
              ("C-M-f" . web-mode-forward-sexp)
              ("C-M-b" . web-mode-backward-sexp)
              ("C-c C-t C-n" . web-mode-tag-next)
              ("C-c C-t C-p" . web-mode-tag-previous)
              ("C-c C-t C-m" . web-mode-tag-match)
              ("C-c C-t C-e" . web-mode-tag-end)
              ("C-c C-s" . nil)
              ("C-c C-l" . nil)
              ("C-c C-d" . nil)
              ("C-=" . web-mode-fold-or-unfold)
              ("C-+" . web-mode-element-children-fold-or-unfold)))

(use-package nodejs-repl
  :hook
  (nodejs-repl-mode . (lambda ()
                        (progn
                          (setq comint-input-ring-file-name "~/.node_repl_history")
                          (comint-read-input-ring 'silent)))))

(use-package sql-mode :ensure nil)

(use-package eglot
  :after (fnm)
  :commands (eglot eglot-ensure eglot-shutdown-all)
  :custom
  (eglot-events-buffer-config '(:size 0 :format full))
  :config
  (fnm-use)
  (add-to-list 'eglot-server-programs
               `((js2-mode js-mode js-ts-mode typescript-ts-mode typescript-mode tsx-ts-mode)
                 . ("typescript-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs
               `((json-mode)
                 . ("vscode-json-languageserver" "--stdio")))
  (add-to-list 'eglot-server-programs
               '(sql-mode . (eglot-sqls "sqls" "-config" ".sqls-config")))
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
  (add-to-list 'eglot-server-programs '((roc-ts-mode) "roc_language_server"))

  (cl-defmethod project-root ((project (head eglot-project)))
    (cdr project))
  :bind
  (:map eglot-mode-map
        ("C-c C-f" . eglot-format)
        ("C-c C-n" . eglot-rename)
        ("M-n" . forward-paragraph)
        ("M-p" . backward-paragraph)
        ("M-." . xref-find-definitions)
        ("M-?" . xref-find-references)
        ("C-<" . eglot-find-typeDefinition)
        ("C-c C-a" . eglot-code-actions)
        ("C-c ! l" . flymake-show-buffer-diagnostics)
        ("C-c ! n" . flymake-goto-next-error)
        ("C-c ! p" . flymake-goto-prev-error))
  :hook
  ((c++-mode c++-ts-mode c-mode c-ts-mode typescript-ts-mode tsx-ts-mode python-mode rust-mode json-mode sql-mode haskell-mode roc-ts-mode) . eglot-ensure)
  (eglot-managed-mode .  (lambda ()
                           (when (or (eq (derived-mode-p major-mode) 'typescript-ts-mode)
                                     (eq (derived-mode-p major-mode) 'js-ts-mode))
                             (flymake-eslint-enable))))
)

(use-package eglot-sqls
  :demand t
  :after eglot
  :ensure nil)

(use-package eldoc-box
  :after eglot
  :bind (:map eglot-mode-map ("C->" . eldoc-box-help-at-point)))

(use-package consult-eglot
  :after eglot)

(use-package flymake-eslint
  :demand t
  :ensure t
  :after flyamke
  :hook
  (typescript-ts-mode . flymake-eslint-enable)
  (typescript-js-mode . flymake-eslint-enable))


(defun eslint-fix ()
  "Format the current file with ESLint."
  (interactive)
  (unless buffer-file-name
    (error "ESLint requires a file-visiting buffer"))
  (when (buffer-modified-p)
    (if (y-or-n-p (format "Save file %s? " buffer-file-name))
        (save-buffer)
      (error "ESLint may only be run on an unmodified buffer")))

  (let* ((default-directory (project-root (project-current t)))
         ;; (eslint-fix-executable "eslint")
         (eslint (format "%snode_modules/.bin/eslint" default-directory))
         (options (list "--fix" buffer-file-name)))
    (unless eslint
      (error "Executable ‘%s’ not found" eslint-fix-executable))
    (apply #'call-process eslint nil 0 nil options)
    (revert-buffer nil t t)))

(if (boundp 'smartparens-mode)
    (defadvice enable-paredit-mode (after activate)
      (smartparens-mode -1)))

(use-package prettier
  :hook (typescript-ts-mode js2-mode))

(defun preserve-paredit-and-enable-ret ()
  (interactive)
  (let* ((enter-key (kbd "RET"))
         (original-binding (key-binding enter-key)))
    (funcall 'enable-paredit-mode)
    (local-set-key enter-key original-binding)))

(use-package paredit
  :hook
  ;; (eval-expression-minibuffer-setup . preserve-paredit-and-enable-ret)
  (emacs-lisp-mode . enable-paredit-mode)
  (slime-mode . enable-paredit-mode)
  (slime-repl-mode . enable-paredit-mode)
  (common-lisp-mode . enable-paredit-mode)
  (lisp-mode . enable-paredit-mode)
  (lisp-data-mode . enable-paredit-mode)
  ;; (minibuffer-mode . preserve-paredit-and-enable-ret)
  ;; (racket-mode . enable-paredit-mode)
  (eshell-mode  . enable-paredit-mode)
  :bind
  (:map paredit-mode-map
        ("C-'" . sp-rewrap-sexp)
        ("M-W" . sp-copy-sexp)
        ("M-?" . xref-find-references)))

(use-package smartparens
  :config
  (setq sp-ignore-modes-list
        '(minibuffer-inactive-mode emacs-lisp-mode eval-expression-minibuffer-setup common-lisp-mode lisp-mode sly-mode))
  (require 'smartparens-config)
  (sp-local-pair 'typescript-mode "<" ">" :trigger-wrap "<")
  (sp-local-pair 'typescript-ts-mode "<" ">" :trigger-wrap "<")
  :hook
  (prog-mode text-mode comint-mode)
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

(use-package yasnippet-snippets)

(use-package yasnippet-capf
  :disabled t
  :demand t
  :after yasnippet
  :hook
  (emacs-lisp-mode . (lambda ()
                       (setq-local completion-at-point-functions
                                   (list (cape-capf-super
                                          #'elisp-completion-at-point
                                          #'yasnippet-capf
                                          #'cape-file)))))
  (sly-mode . (lambda ()
                (setq-local completion-at-point-functions
                            (list (cape-capf-super
                                   #'yasnippet-capf
                                   #'sly-complete-filename-maybe
                                   #'sly-complete-symbol)))))
  (eglot-managed-mode . (lambda ()
                          (setq-local completion-at-point-functions
                                      (list (cape-capf-super
                                             #'eglot-completion-at-point
                                             #'yasnippet-capf
                                             #'cape-file))))))

(use-package yasnippet
  :custom
  (yas-wrap-around-region t)
  (yas-also-auto-indent-first-line t)
  :hook
  (prog-mode . yas-minor-mode-on)
  (emacs-lisp-mode . yas-minor-mode-on)
  (js2-mode . yas-minor-mode-on)
  (typescript-mode . yas-minor-mode-on)
  (js-ts-mode . yas-minor-mode-on)
  (typescript-ts-mode . yas-minor-mode-on)
  (web-mode . yas-minor-mode-on)
  (text-mode . yas-minor-mode-on)
  (lisp-mode . yas-minor-mode-on)
  (inf-mongo-mode . yas-minor-mode-on)
  (rust-mode . yas-minor-mode-on)
  (roc-ts-mode . yas-minor-mode-on)
  :bind (:map yas-minor-mode-map
              ("C-<tab>" . yas-expand))
  :config
  (setq yas-snippet-dirs
        `(,(concat user-emacs-directory "snippets")
          ,yasnippet-snippets-dir))
  (yas-reload-all))

(use-package common-lisp-snippets)

(use-package yaml-mode
  :mode (("\\.gotmpl\\'" . yaml-mode)))

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)
         ("\\.mess\\'" . markdown-mode))
  :init (setq markdown-command "pandoc"))

(use-package restclient
  :init
  (add-to-list 'auto-mode-alist '("\\.client$" . restclient-mode))
  :if (window-system))

(use-package json-mode)
;; (use-package jq-format
;;   :after json-mode)

;; (use-package outline
;;   :ensure nil
;;   :hook ((emacs-lisp-mode eval-expression-minibuffer-setup common-lisp-mode lisp-mode sly-mode lisp-interaction-mode) . outline-minor-mode)
;;   :bind (:map outline-minor-mode-map
;;               ("C-=" . outline-cycle)
;;               ("C-+" . outline-show-all)))

(use-package sly
  :custom
  (inferior-lisp-program "sbcl")
  :bind (:map sly-editing-mode-map
              ("C-c M-c" . avy-goto-word-1)))

(use-package sly-repl-ansi-color
  :after (sly)
  :init (push 'sly-repl-ansi-color sly-contribs))

(use-package sly-asdf)

(use-package sly-quicklisp)

(use-package slite
  :disabled t
  :after (sly)
  :ensure nil
  :load-path "~/common-lisp/slite/")

(use-package hcl-mode
  :disabled t
  :mode
  ("\\.dsl\\'" . hcl-mode))

(use-package pandoc-mode)
(use-package git-link)

(use-package copilot
  :disabled t
  :after (fnm corfu)
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t
  :custom
  (copilot-node-executable (fnm-node-path "16"))
  :bind
  (:map copilot-completion-map
        ("C-<tab>" . copilot-accept-completion))
  (:map corfu-mode-map
        ("C-<tab>" . copilot-accept-completion)))

(use-package racket-mode
  :disabled t
  :hook
  (racket-mode . racket-xp-mode)
  (racket-mode . flymake-racket-setup))

(use-package dape
  :straight (dape :type git :host github :repo "svaante/dape")
  :custom
  ;; (dape--debug-on '())
  (dape-info-buffer-window-groups '((dape-info-scope-mode dape-info-watch-mode)))
  :bind (:map dape-global-map
              ("e" . dape-evaluate-expression))
  :config
  (add-to-list 'dape-configs
               `(vscode-ts-js-attach
                 modes (js-mode js-ts-mode typescript-ts-mode)
                 host "localhost"
                 port 8123
                 command "node"
                 ;; command-cwd "~/source/vscode-js-debug/dist/"
                 command-cwd "~/.emacs.d/debug-adapters/js-debug"
                 command-args ("src/dapDebugServer.js")
                 :port bob/get-inspect-port
                 :sourceMaps t
                 :resolveSourceMapLocations ["**/dist/**/*"]
                 :cwd dape-cwd-fn
                 :autoAttachChildProcesses t
                 :type "pwa-node"
                 :request "attach"
                 :outputCapture "console"
                 :sourceMapRenames t
                 :autoAttachChildProcesses t
                 :console "internalConsole"
                 :killBehavior "forceful"))

  (add-to-list 'dape-configs
               `(ts-node-attach
                 modes (js-mode js-ts-mode typescript-ts-mode)
                 host "localhost"
                 port 8123
                 command "node"
                 command-cwd "~/.emacs.d/debug-adapters/js-debug"
                 command-args ("src/dapDebugServer.js")
                 :sourceMaps t
                 :resolveSourceMapLocations ["**","!**/node_modules/**"]
                 :cwd dape-cwd-fn
                 :autoAttachChildProcesses t
                 :type "pwa-node"
                 :request "attach"
                 :outputCapture "console"
                 :sourceMapRenames t
                 :autoAttachChildProcesses t
                 :console "internalConsole"
                 :killBehavior "forceful"))
  
  (add-to-list 'dape-configs
               `(rust-mode-debug
                 modes (rust-mode)
                 :cwd dape-cwd-fn
                 :type "lldb-dap"
                 :request "launch"
                 ))
  ;; To not display info and/or buffers on startup
  (remove-hook 'dape-on-start-hooks 'dape-info)
  (remove-hook 'dape-on-start-hooks 'dape-repl)

  ;; Use n for next etc. in REPL
  (setq dape-repl-use-shorthand t)

  ;; Kill compile buffer on build success
  (add-hook 'dape-compile-compile-hooks 'kill-buffer)
  :hook (dape-active-mode . repeat-mode))

(use-package erefactor
  :ensure t
  :hook (emacs-lisp-mode .
                         (lambda ()
                           (define-key emacs-lisp-mode-map (kbd "C-c C-v") erefactor-map))))

(use-package dumb-jump
  :ensure t
  :config
  (setq dumb-jump-selector 'popup)
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

;; REPL-driven development for NodeJS
(use-package skerrick
  :bind (:map js2-mode-map
         ("C-x C-e" . skerrick-eval-region)))

(use-package pg
  :straight (:host github :repo "emarsden/pg-el"))

(use-package pgmacs
  :straight (:host github :repo "emarsden/pgmacs"))

(use-package haskell-mode)

(use-package realgud)
(use-package realgud-lldb
  :after realgud)

(use-package emacs
  :hook
  (prog-mode . hs-minor-mode)
  :bind (:map prog-mode-map
              ("C-=" . hs-toggle-hiding)))

(use-package c-ts-mode-indent-offset
  :ensure nil
  :custom
  (c-ts-mode-indent-offset 4))

(use-package roc-ts-mode
  :straight (:host gitlab :repo "tad-lispy/roc-ts-mode")
  :hook
  (before-save . (lambda ()
                   (when (equal major-mode 'roc-ts-mode)
                     (roc-ts-format)))))

(use-package blamer
  :custom
  (blamer-idle-time 0.5)
  (blamer-min-offset 70)
  (blamer-max-commit-message-length 200)
  :bind
  (:map prog-mode-map ("C-c b" . blamer-show-posframe-commit-info))
  :custom-face
  (blamer-face ((t
                 :italic t
                 :font "Futura"
                 :height 180
                 :inherit modus-themes-heading-3)))
  :hook
  (prog-mode . (lambda () (blamer-mode 1))))

(use-package rust-mode
  :init
  (setq rust-mode-treesitter-derive t))

(use-package cargo
  :custom
  (cargo-process--command-clippy--additional-args "--fix")
  :hook (rust-mode . (lambda () (cargo-minor-mode 1))))

(provide 'prog)
