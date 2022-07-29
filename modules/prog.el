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
  :commands lsp
  :init

  (setenv "LSP_USE_PLISTS" "true")
  (setq lsp-use-plists t)
  (setq node-version "18.5.0")
  (setq fnm-dir (cadr (s-split "=" (cl-find-if
                               (lambda (s) (s-starts-with-p "FNM_DIR" s))
                               (s-split "\n" (shell-command-to-string "zsh; eval \"$(fnm env --use-on-cd)\"; env | rg FNM"))))))
  (setq fnm-node (concat fnm-dir
                         "/node-versions/v" node-version "/installation/bin/node"))
  (setq fnm-npm (concat fnm-dir
                         "/node-versions/v" node-version "/installation/bin/npm"))
  (setq lsp-clients-typescript-npm-location
        fnm-npm)
  (setenv "NODE_PATH" fnm-node)
  (advice-add 'lsp :before (lambda (&rest _args) (eval '(setf (lsp-session-server-id->folders (lsp-session)) (ht)))))
  :custom
  (lsp-prefer-flymake nil)           ; Use flycheck instead of flymake
  (lsp-file-watch-threshold 2000)
  (lsp-eslint-auto-fix-on-save t)
  (read-process-output-max (* 1024 1024))
  ;; (lsp-eldoc-hook nil)
  (lsp-ui-doc-show-with-cursor t)
  ;; (company-lsp-cache-candidates t)
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

(use-package json-mode)
(use-package jq-format
  :after json-mode)

(use-package origami
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

(use-package sly-asdf
  :disabled t)
(use-package sly-quicklisp
  :disabled t)

(use-package hcl-mode
  :mode
  ("\\.dsl\\'" . hcl-mode))

(use-package pandoc-mode)

(provide 'prog)
