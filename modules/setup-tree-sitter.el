;;; setup-tree-sitter.el --- summary -*- lexical-binding: t -*-
(use-package treesit
  :disabled t
  :ensure nil
  :demand t
  :init
  (setq treesit-language-source-alist
        '((bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
          (c . ("https://github.com/tree-sitter/tree-sitter-c"))
          (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
          (css . ("https://github.com/tree-sitter/tree-sitter-css"))
          (cmake . ("https://github.com/uyha/tree-sitter-cmake"))
          (csharp     . ("https://github.com/tree-sitter/tree-sitter-c-sharp.git"))
          (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile"))
          (elisp . ("https://github.com/Wilfred/tree-sitter-elisp"))
          (go . ("https://github.com/tree-sitter/tree-sitter-go"))
          (gomod      . ("https://github.com/camdencheek/tree-sitter-go-mod.git"))
          (html . ("https://github.com/tree-sitter/tree-sitter-html"))
          (java       . ("https://github.com/tree-sitter/tree-sitter-java.git"))
          (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
          (json . ("https://github.com/tree-sitter/tree-sitter-json"))
          (lua . ("https://github.com/Azganoth/tree-sitter-lua"))
          (make . ("https://github.com/alemuller/tree-sitter-make"))
          (markdown . ("https://github.com/MDeiml/tree-sitter-markdown" nil "tree-sitter-markdown/src"))
          (ocaml . ("https://github.com/tree-sitter/tree-sitter-ocaml" nil "ocaml/src"))
          (org . ("https://github.com/milisims/tree-sitter-org"))
          (python . ("https://github.com/tree-sitter/tree-sitter-python"))
          (php . ("https://github.com/tree-sitter/tree-sitter-php"))
          (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src"))
          (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "tsx/src"))
          (ruby . ("https://github.com/tree-sitter/tree-sitter-ruby"))
          (rust . ("https://github.com/tree-sitter/tree-sitter-rust"))
          (sql . ("https://github.com/derekstride/tree-sitter-sql" "gh-pages"))
          (vue . ("https://github.com/merico-dev/tree-sitter-vue"))
          (yaml . ("https://github.com/ikatyang/tree-sitter-yaml"))
          (toml . ("https://github.com/tree-sitter/tree-sitter-toml"))
          (zig . ("https://github.com/GrayJack/tree-sitter-zig")))

        major-mode-remap-alist
        '((c-mode          . c-ts-mode)
          (c++-mode        . c++-ts-mode)
          (c-or-c++-mode   . c-or-c++-ts-mode)
          (cmake-mode      . cmake-ts-mode)
          (conf-toml-mode  . toml-ts-mode)
          (css-mode        . css-ts-mode)
          (js-mode         . js-ts-mode)
          (java-mode       . java-ts-mode)
          (js-json-mode    . json-ts-mode)
          (python-mode     . python-ts-mode)
          (sh-mode         . bash-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (rust-mode       . rust-ts-mode)))

  (add-to-list 'auto-mode-alist '("CMakeLists\\'" . cmake-ts-mode))
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.y[a]?ml\\'" . yaml-ts-mode)))

(use-package treesit-auto
  :disabled t
  :after (treesit)
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))


(use-package tree-sitter
  :hook (js-mode typescript-mode c-mode c++-mode rust-mode)
  :config
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs)

(use-package ts-fold
  :straight (ts-fold :type git :host github :repo "emacs-tree-sitter/ts-fold")
  :init
  :hook ((js-mode typescript-mode c-mode c++-mode rust-mode) . ts-fold-mode)
  :bind (:map ts-fold-mode
              ("C-=" . ts-fold-toggle)
              ("C-+" . ts-fold-open-recursively)))

(provide 'setup-tree-sitter)
