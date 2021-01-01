;;; 'setup-web-dev.el --- summary -*- lexical-binding: t -*-

(use-package web-mode
  :mode
  ("\\.html\\'" . web-mode)
  ("\\.cssl\\'" . web-mode)
  ("\\.jsx\\'" . web-mode)
  ("\\.vue\\'" . web-mode)
  :hook
  (web-mode . yas-minor-mode)
  ;; (web-mode . highlight-indent-guides-mode)
  ;; (web-mode . emojify-mode)
  (web-mode . flycheck-mode)
  (web-mode . (lambda ()
                (flycheck-select-checker 'javascript-eslint)))
  (web-mode . add-node-modules-path)
  (web-mode . lsp)
  (web-mode . eldoc-mode)
  :config
  (flycheck-add-mode 'javascript-eslint 'web-mode)
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
  (setq web-mode-enable-auto-indentation t)
  (setq web-mode-enable-auto-expanding t)
  (setq vetur.validation.template t) ;; For lsp-vue
  (setq lsp-vetur-dev-log-level "debug")
  ;; (nvm-use "v12.14.0")
  :bind (:map web-mode-map
              ("C-c C-t C-n" . web-mode-tag-next)
              ("C-c C-t C-p" . web-mode-tag-previous)
              ("C-c C-t C-m" . web-mode-tag-match)
              ("C-c C-t C-e" . web-mode-tag-end)
              ("C-=" . origami-toggle-node)
              ("C-c C-s" . nil)) ;; Unbind insert snippet so deadgrep C-c C-s C-d will work
)

(use-package emojify)
(provide 'setup-web-dev)

;;; web-dev.el ends here
