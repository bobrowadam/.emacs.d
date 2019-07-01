(use-package cargo
  :if (window-system)
  :ensure t)

(use-package rust-mode
  :if (window-system)
  :hook
  (rust-mode . cargo-minor-mode)
  (rust-mode . flycheck-rust-setup)
  (rust-mode . flycheck-mode)
  (rust-mode . highlight-indent-guides-mode)
  (rust-mode . eldoc-mode)
  (rust-mode . racer-mode)
  (rust-mode . (lambda () (yas-load-directory (concat user-emacs-directory "snippets/rust-mode/"))))
  :bind (:map rust-mode-map ("TAB" . #'company-indent-or-complete-common))
  :config
  (setq rust-format-on-save t)
  (setq company-tooltip-align-annotations t))

(use-package racer
  :if (window-system)
  :after rust-mode
  :config (setq racer-rust-src-path "/Users/bob/.rustup/toolchains/stable-x86_64-apple-darwin/lib/rustlib/src/rust/src"))

(use-package lsp-rust
  :disabled t
  :demand t
  :hook
  (rust-mode . lsp-rust-enable))

(provide 'setup-rust)
