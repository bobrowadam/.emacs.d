(use-package cargo
  :if (window-system)
  :ensure t)

(use-package flycheck-rust)

(use-package rust-mode
  :demand
  :if (window-system)
  :hook
  (rust-mode . cargo-minor-mode)
  (rust-mode . flycheck-rust-setup)
  (rust-mode . flycheck-mode )
  (rust-mode . highlight-indent-guides-mode)
  (rust-mode . yas-minor-mode)
  (rust-mode . eldoc-mode)
  ;; (rust-mode . racer-mode)
  (rust-mode . lsp)
  (rust-mode . (lambda () (yas-load-directory (concat user-emacs-directory "snippets/rust-mode/"))))
  :bind (:map rust-mode-map
              ("TAB" . #'company-indent-or-complete-common)
              ("C-=". origami-toggle-node))
  :config
  (setq rust-format-on-save t)
  (setq company-tooltip-align-annotations t)
  ;; This is to prevent cargo projects slip into projectile known projects
  (defun projectile-ignore-cargo (project-name)
    (s-contains? ".cargo" project-name))
  (setq projectile-ignored-project-function 'projectile-ignore-cargo)
  ;; (setq lsp-prefer-capf t)
  (setq lsp-completion-provider :capf)
  (setq lsp-completion-enable t)
  ;; (setenv "PATH" "~/.local/bin:/Users/bob/.cargo/bin:~/bin:/usr/local/bin:/usr/bin")
  (setq exec-path (append exec-path '("/Users/bob/bin" "~/bob/.local/bin" "")))
  (setq lsp-rust-analyzer-server-command "/Users/zerok/.local/bin/rust-analyzer")
)

(use-package racer
  :disabled t
  :if (window-system)
  :after rust-mode
  :config (setq racer-rust-src-path "/Users/bob/.rustup/toolchains/stable-x86_64-apple-darwin/lib/rustlib/src/rust/src"))

(provide 'setup-rust)
