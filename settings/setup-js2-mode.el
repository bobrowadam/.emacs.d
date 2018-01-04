;;; setup-js2-mode.el --- tweak js2 settings -*- lexical-binding: t; -*-
(use-package js2-mode
  :ensure t
  :init
  (setq-default js2-allow-rhino-new-expr-initializer nil)
  (setq-default js-indent-level 2)
  (setq-default js2-basic-offset 2)
  (setq-default js2-auto-indent-p nil)
  (setq-default js2-enter-indents-newline nil)
  (setq-default js2-global-externs '("module" "require" "buster" "sinon" "assert" "refute" "setTimeout" "clearTimeout" "setInterval" "clearInterval" "location" "__dirname" "console" "JSON"))
  (setq-default js2-idle-timer-delay 0.1)
  (setq-default js2-indent-on-enter-key nil)
  (setq-default js2-mirror-mode nil)
  (setq-default js2-strict-inconsistent-return-warning nil)
  (setq-default js2-auto-indent-p t)
  (setq-default js2-include-rhino-externs nil)
  (setq-default js2-include-gears-externs nil)
  (setq-default js2-concat-multiline-strings 'eol)
  (setq-default js2-rebind-eol-bol-keys nil)

  (setq-default js2-show-parse-errors nil)
  (setq-default js2-strict-missing-semi-warning nil)
  (setq-default js2-strict-trailing-comma-warning nil)
  (setq-default js2-bounce-indent-p nil)
  (add-to-list 'auto-mode-alist (cons (rx ".js" eos) 'js2-mode))
  (add-hook 'js2-mode-hook '(lambda () (flyspell-prog-mode)))
  (add-hook 'js2-mode-hook '(lambda ()
                              (define-key js2-mode-map (kbd "C-<tab>") 'js2-indent-bounce )))
  (add-hook 'js2-mode-hook 'prettify-symbols-mode)

  (use-package highlight-indent-guides
    :ensure t
    :config (add-hook 'js2-mode-hook 'highlight-indent-guides-mode))

  (add-hook 'js2-mode-hook 'js2-mode-hide-warnings-and-errors)
  ;; :bind
  ;; (:map js2-mode-map
  ;;       ("C-c TAB" . js2-indent-bounce))
  :config
  (use-package pretty-mode
    :disabled
    :ensure t
    :config (add-hook 'js2-mode-hook 'turn-on-pretty-mode))

  (js2-imenu-extras-mode))

;; Tern.JS
;; (add-to-list 'load-path (expand-file-name "tern/emacs" site-lisp-dir))
;; (autoload 'tern-mode "tern.el" nil t)

(use-package color-identifiers-mode
  :disabled
  :ensure t
  :config
  (add-hook 'js2-mode-hook 'color-identifiers-mode))

(use-package json-mode
  :ensure t)

;; Tern
(autoload 'tern-mode "tern.el" nil t)
(add-hook 'js2-mode-hook (lambda () (tern-mode t)
                           (company-mode)))

(use-package tern
  :ensure t
  :config

  (use-package company-tern
    :ensure t
    :config
    ()
    )
  
  (use-package tern-auto-complete
    :disabled
    :ensure t
    :config
    (tern-ac-setup)))

(use-package nvm
  :ensure t)

(use-package add-node-modules-path
  :ensure t
  :config
  (eval-after-load 'js2-mode
    '(add-hook 'js2-mode-hook #'add-node-modules-path)))

;; disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(json-jsonlist)))

;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint)))

(use-package indium
  :ensure t)

(provide 'setup-js2-mode)
