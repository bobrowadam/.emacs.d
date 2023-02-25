(use-package add-node-modules-path)
(use-package flycheck)

(use-package flycheck-posframe
  :ensure t
  :after flycheck
  :config (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode))

(use-package rust-mode)

(use-package cargo-mode)

(use-package fancy-compilation
  :config
  (setq fancy-compilation-term "xterm-256color")
  :commands (fancy-compilation-mode))

(with-eval-after-load 'compile
  (fancy-compilation-mode))

(defun npm-run-build ()
  "Build typescript project on watch mode"
  (interactive)
  (when (and (project-current) (eq major-mode 'typescript-mode))
    (let ((default-directory (project-root (project-current t)))
          (comint-scroll-to-bottom-on-input t)
          (comint-scroll-to-bottom-on-output t)
          (comint-process-echoes t))
      (compilation-start (format "%s ./node_modules/typescript/bin/tsc -w" (fnm-node-path "18")) t))))

(defun npm-install-project (&optional force)
  "NPM install in project.
If FORCE is non-nil, delete the 'package-lock.json' and 'node_modules' directories and verify NPM cache
before running 'npm install'."
  (interactive "P")
  (let* ((default-directory (project-root (project-current t)))
         (project-node-version (read-file ".nvmrc"))
         (local-npm-executable (fnm-npm-path project-node-version)))
    (message "local NPM executable version is %s" (bobs-shell-comand-to-string local-npm-executable "-v"))
    (when force
      (message "removing package-lock.json")
      (unwind-protect (delete-file (concat default-directory "package-lock.json")))
      (message "removing node_modules")
      (unwind-protect (delete-directory (concat default-directory "node_modules") t))
      (message "verifying NPM's cache")
      (apply #'call-process local-npm-executable nil 0 nil '("verify")))
    (start-process "npm-install" "*npm-install-output*" local-npm-executable "install")
    (split-window-horizontally)
    (switch-to-buffer (get-buffer "*npm-install-output*"))))

(use-package typescript-mode
  :demand t
  :hook
  (typescript-mode . add-node-modules-path)
  (typescript-mode . eldoc-mode)
  :bind (:map typescript-mode-map ("C-c C-b" . npm-run-build))
  :config
  (setq-default typescript-indent-level 2))

(use-package jest-test-mode
  :init
  :commands jest-test-mode
  :hook (typescript-mode js-mode typescript-tsx-mode))

(use-package js2-mode
  :demand t
  :init
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  :hook
  (js2-mode . add-node-modules-path)
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
  ("\\.jsx\\'" . web-mode)
  ("\\.vue\\'" . web-mode)
  :hook
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
  :bind (:map web-mode-map
              ("C-c C-t C-n" . web-mode-tag-next)
              ("C-c C-t C-p" . web-mode-tag-previous)
              ("C-c C-t C-m" . web-mode-tag-match)
              ("C-c C-t C-e" . web-mode-tag-end)
              ("C-c C-s" . nil)
              ("C-c C-l" . nil)))

(use-package nodejs-repl
  :hook
  (nodejs-repl-mode . (lambda ()
                             (progn
                               (setq comint-input-ring-file-name "~/.node_repl_history")
                               (comint-read-input-ring 'silent)))))

(use-package eglot
  :demand t
  :config
  (setenv "NODE_PATH" "/Users/bob/Library/Application Support/fnm/node-versions/v18.13.0/installation/lib/node_modules")
  (add-to-list 'eglot-server-programs
               `((js-mode typescript-mode)
                                 . ("/Users/bob/Library/Application Support/fnm/node-versions/v18.13.0/installation/bin/typescript-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs
               `((web-mode) . ("/Users/bob/Library/Caches/fnm_multishells/77292_1674653900807/bin/vls" "--stdio")))
  ;; (add-to-list 'eglot-server-programs '(web-mode . ("/Users/bob/Library/Application Support/fnm/node-versions/v18.13.0/installation/bin/vue-language-server" "--stdio"
  ;;                                                   ;; :initializationOptions
  ;;                                                   ;; (:typescript (:tsdk "node_modules/typescript/lib"))
  ;;                                                   )))
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
        ("C-c C-a" . eglot-code-actions))
  :hook
  ((js2-mode typescript-mode web-mode) . eglot-ensure))

(use-package flymake
  :demand t
  :hook
  (typescript-mode . flymake-mode)
  (js2-mode . flymake-mode)
  :bind (:map flymake-mode-map
              ("C-c !" . flymake-show-buffer-diagnostics)))

(defun set-eslint-executable ()
  (when
   (setq flymake-eslint-executable-name)))

(defun enable-flymake-after-eglot ()
  (progn
    (setq flymake-eslint-project-root (project-root (project-current t)))
    (setq flymake-eslint-executable-name (format "%snode_modules/.bin/eslint" flymake-eslint-project-root))
    (setq temp-before-hook eglot-managed-mode-hook)
    (add-hook 'eglot-managed-mode-hook
              (lambda ()
                (flymake-eslint-enable)
                (setq eglot-managed-mode-hook temp-before-hook)))))

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
           (eslint (format "%snode_modules/.bin/eslint" flymake-eslint-project-root))
           (options (list "--fix" buffer-file-name)))
      (unless eslint
        (error "Executable ‘%s’ not found" eslint-fix-executable))
      (apply #'call-process eslint nil 0 nil options)
      (revert-buffer t t t)))


(use-package flymake-eslint
  :after (eglot)
  :demand t
  :hook
  (after-save . (lambda ()
                  (cond ((eq major-mode 'typescript-mode) (eslint-fix))
                        ((eq major-mode 'js2-mode) (eslint-fix)))))
  (typescript-mode . enable-flymake-after-eglot)
  (js2-mode . enable-flymake-after-eglot))

(defadvice enable-paredit-mode (after activate)
  (smartparens-mode -1))

(use-package paredit
  :hook
  ;; (eval-expression-minibuffer-setup . enable-paredit-mode)
  (emacs-lisp-mode . enable-paredit-mode)
  (slime-mode . enable-paredit-mode)
  (slime-repl-mode . enable-paredit-mode)
  (common-lisp-mode . enable-paredit-mode)
  (lisp-mode . enable-paredit-mode)
  (lisp-data-mode . enable-paredit-mode)
  ;; (eshell-mode  . enable-paredit-mode)
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
  (inf-mongo-mode . yas-minor-mode-on)
  (rust-mode . yas-minor-mode-on)
  :config
  (setq yas-snippet-dirs
        `(,(concat user-emacs-directory "snippets")
          ,yasnippet-snippets-dir))
  (yas-reload-all))

(use-package common-lisp-snippets)

(use-package yaml-mode)
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)
         ("\\.mess\\'" . markdown-mode))
  :init (setq markdown-command "pandoc"))

(use-package restclient
  :init
  (add-to-list 'auto-mode-alist '("\\.client$" . restclient-mode))
  :if (window-system))

(use-package rainbow-delimiters
  :disabled t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package tree-sitter
  :hook ((js2-mode typescript-mode
                   c-mode c++-mode rust-mode
                   ) . tree-sitter-hl-mode)
  :ensure t)

(use-package tree-sitter-langs
  :ensure t
  :demand t
  :after tree-sitter)

(use-package json-mode)
(use-package jq-format
  :after json-mode)

(use-package origami
  :demand t
  :hook
  (prog-mode . origami-mode)
  :bind (:map origami-mode-map
              ("C-=" . origami-recursively-toggle-node)
              ("C-+" . origami-toggle-all-nodes)))

(use-package haskell-mode :disabled t)
(use-package haskell-snippets :disabled t)
(use-package cider :disabled t)
(use-package clojure-mode :disabled t)

(use-package sly
  :demand t
  :init
  (setq inferior-lisp-program "sbcl")
  :bind (:map sly-editing-mode-map
              ("C-c M-c" . avy-goto-word-1))
  :hook
  (sly-mode . (lambda ()
     (unless (sly-connected-p)
       (save-excursion (sly))))))

(use-package sly-repl-ansi-color
  :after (sly)
  :init (push 'sly-repl-ansi-color sly-contribs))

(use-package sly-asdf
  :disabled t)

(use-package sly-quicklisp
  :disabled t)

(use-package slite
  :after (sly)
  :ensure nil
  :load-path "~/common-lisp/slite/")

(use-package hcl-mode
  :mode
  ("\\.dsl\\'" . hcl-mode))

(use-package pandoc-mode)
(use-package git-link)

(use-package chatgpt
  :disabled t
  :straight (:host github :repo "joshcho/ChatGPT.el" :files ("dist" "*.el"))
  :init
  (require 'python)
  (setq python-interpreter "python3")
  (setq chatgpt-repo-path "~/.emacs.d/straight/repos/ChatGPT.el/")
  :bind ("C-c q" . chatgpt-query))

(provide 'prog)
