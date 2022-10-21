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
      (compilation-start "npm run build -- -w" t))))

(use-package typescript-mode
  :hook
  (before-save . (lambda ()
                   (when
                       (and (eq 'typescript-mode major-mode)
                            (bound-and-true-p lsp-mode))
                     (lsp-eslint-apply-all-fixes))))
  (typescript-mode . add-node-modules-path)
  (typescript-mode . origami-mode)
  (typescript-mode . eldoc-mode)
  :bind (:map typescript-mode-map ("C-c C-b" . npm-run-build))
  :config
  (setq typescript-indent-level 2))

(use-package jest-test-mode
  :init
  :commands jest-test-mode
  :hook (typescript-mode js-mode typescript-tsx-mode))

(use-package js2-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  :hook
  (before-save . (lambda ()
                   (when
                       (and (eq 'js2-mode major-mode)
                            (bound-and-true-p lsp-mode))
                     (lsp-eslint-apply-all-fixes))))
  (js2-mode . add-node-modules-path)
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

(use-package web-mode
  :mode
  ("\\.html\\'" . web-mode)
  ("\\.cssl\\'" . web-mode)
  ("\\.jsx\\'" . web-mode)
  ("\\.vue\\'" . web-mode)
  :hook
  (before-save . (lambda ()
                   (when
                       (and (eq 'web-mode major-mode)
                            (bound-and-true-p lsp-mode))
                     (lsp-eslint-apply-all-fixes))))
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
  :bind (:map web-mode-map
              ("C-c C-t C-n" . web-mode-tag-next)
              ("C-c C-t C-p" . web-mode-tag-previous)
              ("C-c C-t C-m" . web-mode-tag-match)
              ("C-c C-t C-e" . web-mode-tag-end)
              ("C-c C-s" . nil)
              ("C-c C-l" . nil)) ;; Unbind insert snippet so deadgrep C-c C-s C-d will work
  )

(use-package nodejs-repl)

(use-package lsp-mode
  :disabled t
  :commands lsp
  :init
  ;; if using company for completion remove this:
  (setq lsp-completion-provider :none)
  (setenv "LSP_USE_PLISTS" "true")
  (setq lsp-use-plists nil)
  (setq lsp-eldoc-render-all t)
  (setq lsp-log-io t)
  (setq lsp-clients-typescript-log-verbosity "verbose")
  (setq lsp-typescript-tsserver-log "verbose")
  (setq lsp-typescript-tsserver-trace "verbose")
  :custom
  (lsp-prefer-flymake nil)           ; Use flycheck instead of flymake
  (lsp-file-watch-threshold 2000)
  (lsp-eslint-auto-fix-on-save t)
  (read-process-output-max (* 1024 1024))
  ;; (lsp-eldoc-hook nil)
  ;; (lsp-javascript-display-enum-member-value-hints t)
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
                   ) . lsp-deferred))

(use-package lsp-ui
  :after (lsp-mode)
  :init
  (setq lsp-ui-sideline-enable nil)
  :hook ((js2-mode typescript-mode web-mode
                   c-mode c++-mode rust-mode
                   ) . lsp-ui-mode)
  ;; :bind
  ;; (:map lsp-mode-map
  ;;       (""))
  )

(use-package eglot
  :init
  ;; (setq node-version "18.9.0")
  ;; (setq fnm-dir (cadr (s-split "=" (cl-find-if
  ;;                              (lambda (s) (s-starts-with-p "FNM_DIR" s))
  ;;                              (s-split "\n" (shell-command-to-string "zsh; eval \"$(fnm env --use-on-cd)\"; env | rg FNM"))))))
  ;; (setq fnm-node (concat fnm-dir
  ;;                        "/node-versions/v" node-version "/installation/bin/node"))
  ;; (setq fnm-npm (concat fnm-dir
  ;;                        "/node-versions/v" node-version "/installation/bin/npm"))
  ;; (setq lsp-clients-typescript-npm-location
  ;;       fnm-npm)
  ;; (setenv "NODE_PATH" fnm-node)
  ;; (add-to-list 'eglot-server-programs
  ;;              `((js-mode typescript-mode)
  ;;                                . (,fnm-node "typescript-language-server" "--stdio")))
  (cl-defmethod project-root ((project (head eglot-project)))
    (cdr project))
  :bind
  (:map eglot-mode-map
        ("C-c C-f" . eglot-format)
        ("C-c C-n" . eglot-rename)
        ("M-." . eglot-find-typeDefinition)
        ("M-n" . forward-paragraph)
        ("M-p" . backward-paragraph)
        ("C-c !" . consult-flymake)
        ("C-c C-a" . eglot-code-actions))
  :hook ((js2-mode typescript-mode web-mode
                   ) . eglot-ensure))

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
        (eslint-fix-executable "eslint")
        (eslint (executable-find eslint-fix-executable))
        (options (list "--fix" buffer-file-name)))
    (unless eslint
      (error "Executable ‘%s’ not found" eslint-fix-executable))
    (apply #'call-process eslint nil "*ESLint Errors*" nil options)
    (revert-buffer t t t)))

(use-package flymake-eslint
  :hook
  (typescript-mode . (lambda () (setq flymake-eslint-project-root (project-root (project-current t)))))
  (typescript-mode . flymake-eslint-enable)
  (js2-mode . (lambda () (setq flymake-eslint-project-root (project-root (project-current t)))))
  (js2-mode . (lambda () (progn
                           (print "before flymake-eslint-enable")
                           (flymake-eslint-enable)))))

(defadvice enable-paredit-mode (after activate)
  (smartparens-mode -1))

(use-package paredit
  :hook
  (eval-expression-minibuffer-setup . enable-paredit-mode)
  (emacs-lisp-mode . enable-paredit-mode)
  (slime-mode . enable-paredit-mode)
  (slime-repl-mode . enable-paredit-mode)
  (common-lisp-mode . enable-paredit-mode)
  (lisp-mode . enable-paredit-mode)
  (lisp-data-mode . enable-paredit-mode)
  (eshell-mode  . enable-paredit-mode)
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
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc"))

(use-package restclient
  :init
  (add-to-list 'auto-mode-alist '("\\.client$" . restclient-mode))
  :if (window-system))

(use-package rainbow-delimiters
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
  :bind (:map origami-mode-map
              ("C-=" . origami-toggle-node)))

(use-package haskell-mode :disabled t)
(use-package haskell-snippets :disabled t)
(use-package cider :disabled t)
(use-package clojure-mode :disabled t)

(use-package sly
  :init
  (setq inferior-lisp-program "/usr/local/bin/sbcl")
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

(use-package hcl-mode
  :mode
  ("\\.dsl\\'" . hcl-mode))

(use-package pandoc-mode)

(use-package lsp-bridge
  :disabled t
  :ensure nil
  :load-path "~/source/lsp-bridge/"
  :init
  (setq lsp-bridge-enable-log t)
  :bind
  (:map lsp-bridge-mode-map
        ("C-c C-f" . lsp-bridge-code-format)
        ("C-c C-n" . lsp-bridge-rename)
        ("M-." . lsp-bridge-find-def)
        ("M-," . lsp-bridge-return-from-def)
        ("C-c C-r" . lsp-bridge-find-references)
        ("M-n" . forward-paragraph)
        ("M-p" . backward-paragraph)
        ("C-c !" . consult-flymake)
        ("C-c C-a" . lsp-bridge-code-action)
        ("C-c C-d" . lsp-bridge-lookup-documentation)
        ("C->" . lsp-bridge-popup-documentation-scroll-up)
        ("C-<" . lsp-bridge-popup-documentation-scroll-down))
  :hook ((js2-mode typescript-mode web-mode
                   ) . lsp-bridge-mode))

;; (require 'lsp-bridge)
(provide 'prog)
