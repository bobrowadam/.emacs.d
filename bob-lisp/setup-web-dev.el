;;; 'setup-web-dev.el --- summary -*- lexical-binding: t -*-

(use-package web-mode
  :mode
  ("\\.html\\'" . web-mode)
  ("\\.cssl\\'" . web-mode)
  ("\\.jsx\\'" . web-mode)
  ("\\.vue\\'" . web-mode)
  :hook
  (web-mode . yas-minor-mode)
  (web-mode . highlight-indent-guides-mode)
  (web-mode . emojify-mode)
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-attr-indent-offset 2)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-style-padding 1)
  (setq web-mode-script-padding 1)
  (setq web-mode-block-padding 0)
  (setq web-mode-comment-style 2)
  (setq web-mode-enable-css-colorization t)
  (setq web-mode-enable-current-element-highlight t)
  (nvm-use "v12.14.0")
  :bind (:map web-mode-map
              ("C-c C-t C-n" . web-mode-tag-next)
              ("C-c C-t C-p" . web-mode-tag-previous)
              ("C-c C-t C-m" . web-mode-tag-match)
              ("C-c C-t C-e" . web-mode-tag-end))
)

(use-package skewer-mode)
(use-package impatient-mode)
(use-package emojify)
(provide 'setup-web-dev)

;;; web-dev.el ends here
