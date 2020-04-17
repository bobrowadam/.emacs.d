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
  (setq projectile-ignored-project-function 'projectile-ignore-cargo)
  (defun projectile-ignore-cargo (project-name)
    (s-contains? ".cargo" project-name))
)

(use-package racer
  :disabled t
  :if (window-system)
  :after rust-mode
  :config (setq racer-rust-src-path "/Users/bob/.rustup/toolchains/stable-x86_64-apple-darwin/lib/rustlib/src/rust/src"))

(use-package lsp-ui)


(provide 'setup-rust)
