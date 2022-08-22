(defun ibuffer-filter-by-prog-mode  ()
  (ibuffer-filter-by-derived-mode 'prog-mode))

(defun short--file-path (file-path)
  (s-prepend "/" (s-join "/" (-take-last 4 (s-split "/" file-path)))))

(use-package ibuffer
  :ensure nil
  :bind
  ("C-x C-b" . ibuffer)
  ("C-x p I" . project-ibuffer)

  :config
  (require 'project-ibuffer)
  (define-ibuffer-column short-file-name (:name Testing-Define-Column :inline true)
    (if-let ((root-dir (cdr (ibuffer-vc-root (current-buffer))))
             (visiting-file-name (buffer-file-name)))
        (short--file-path (s-replace (expand-file-name root-dir) "" visiting-file-name))
      (or (buffer-file-name) (buffer-name))))
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (cond
     ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
     ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
     (t (format "%8d" (buffer-size)))))
  :custom
  (ibuffer-expert t)
  (ibuffer-show-empty-filter-groups nil)
  (ibuffer-formats
   '((mark modified read-only vc-status-mini " "
           (short-file-name))
     (mark modified read-only vc-status-mini " "
           (name 18 18 :left :elide)
           " "
           (size-h 9 -1 :right)
           " "
           (mode 16 16 :left :elide)
           " "
           filename-and-process)))
  (ibuffer-filter-group-name-face 'font-lock-doc-face)
  :hook
  (ibuffer . (lambda ()
               (ibuffer-auto-mode)
               (ibuffer-vc-set-filter-groups-by-vc-root)
               (ibuffer-filter-by-prog-mode)
               (all-the-icons-ibuffer-mode)
               (unless (eq ibuffer-sorting-mode 'recency)
                 (ibuffer-do-sort-by-recency)))))

(use-package all-the-icons-ibuffer
  :init (all-the-icons-ibuffer-mode 1))

(use-package ibuffer-vc
  :demand
  :commands (ibuffer-vc-set-filter-groups-by-vc-root)
  :custom
  (ibuffer-vc-skip-if-remote t))

(use-package consult
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c m" . consult-mode-command)
         ("C-x r b" . consult-bookmark)
         ("C-c k" . consult-kmacro)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)
         ("C-M-#" . consult-register)
         ("M-y" . consult-yank-pop)
         ("<help> a" . consult-apropos)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-i" . consult-imenu)
         ("M-g i" . consult-imenu-multi)
         ("M-g C-g" . consult-git-grep)
         ("M-g r" . consult-ripgrep)
         ("C-M-s" . consult-line)
         ("C-c M-s e" . consult-isearch)
         :map isearch-mode-map
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi))
  :init
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  (setq consult-narrow-key "<"))

(use-package consult-project-extra
  :bind
  (("C-x p f" . consult-project-extra-find)
   ("C-x p o" . consult-project-extra-find-other-window)))

(use-package project
  :ensure nil
  :bind
  (("C-x p s" . bob/project-vterm)
   ("C-x p m"  . magit-project-status)
   ("C-x p C-m"  . project-dired))
  :init
  (setq project-switch-commands
        '((consult-project-extra-find "Find file")
          (project-dired "Find directory")
          (bob/project-vterm "Vterm")
          (magit-project-status "Magit")
          (consult-ripgrep "Grep" "g")
          (project-ibuffer "Ibuffer" "b")))
  (unless (project-known-project-roots)
    (message "No project file found, indexing projects")
    (progn
      (project-remember-projects-under "~/source/services")
      (project-remember-projects-under "~/source"))))

(use-package consult-lsp :ensure t)

(use-package hotfuzz
  :after selectrum
  :ensure t
  :custom
  (completion-ignore-case t)
  :init
  (hotfuzz-selectrum-mode))

(use-package savehist-mode
  :ensure nil
  :init
  (savehist-mode))

(use-package recentf-mode
  :ensure nil
  :init
  (recentf-mode 1))

(use-package selectrum
  :bind (("C-M-r" . selectrum-repeat)
         :map selectrum-minibuffer-map
         ("C-r" . selectrum-select-from-history)
         :map minibuffer-local-map
         ("M-h" . backward-kill-word))
  :custom
  (selectrum-fix-minibuffer-height t)
  (selectrum-num-candidates-displayed 7)
  ;; (selectrum-refine-candidates-function #'orderless-filter)
  ;; (selectrum-highlight-candidates-function #'orderless-highlight-matches)
  :custom-face
  (selectrum-current-candidate ((t (:background "#3a3f5a"))))
  :init
  (selectrum-mode 1))

(use-package marginalia
  ;; :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)
   ;; ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings))

  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-separator ?\s)          ;; Orderless field separator
  (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  (corfu-preview-current nil)    ;; Disable current candidate preview
  (corfu-preselect-first nil)    ;; Disable candidate preselection
  (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  (corfu-echo-documentation nil) ;; Disable documentation in the echo area
  (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  :hook ((prog-mode . corfu-mode)
         (vterm-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-excluded-modes'.
  :init
  (global-corfu-mode)
  (defun corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      ;; (setq-local corfu-auto nil) Enable/disable auto completion
      (corfu-mode 1)))

  (defun corfu-send-shell (&rest _)
    "Send completion candidate when inside comint/eshell."
    (cond
     ((and (derived-mode-p 'eshell-mode) (fboundp 'eshell-send-input))
      (eshell-send-input))
     ((and (derived-mode-p 'comint-mode)  (fboundp 'comint-send-input))
      (comint-send-input))))

  (advice-add #'corfu-insert :after #'corfu-send-shell)
  :hook
  (minibuffer-setup . corfu-enable-in-minibuffer))

(use-package dabbrev
  :ensure nil
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  ;; Other useful Dabbrev configurations.
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

;; A few more useful configurations...
(use-package emacs
  :ensure nil
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))

(use-package company
  :disabled t
  :if (window-system)
  :init
  (setq company-tooltip-align-annotations t)
  (setq company-minimum-prefix-length 0)
  (setq company-idle-delay 0.3)
  (setq company-candidates-cache t)
  (global-company-mode 1))

(use-package company-box
  :disabled t
  :after company
  :if (display-graphic-p)
  :defines company-box-icons-all-the-icons
  :hook (company-mode . company-box-mode)
  :custom
  (company-box-backends-colors nil)
  :config
  (with-no-warnings
    ;; Prettify icons
    (defun my-company-box-icons--elisp (candidate)
      (when (derived-mode-p 'emacs-lisp-mode)
        (let ((sym (intern candidate)))
          (cond ((fboundp sym) 'Function)
                ((featurep sym) 'Module)
                ((facep sym) 'Color)
                ((boundp sym) 'Variable)
                ((symbolp sym) 'Text)
                (t . nil)))))
    (advice-add #'company-box-icons--elisp :override #'my-company-box-icons--elisp))

  (when (and (display-graphic-p)
             (require 'all-the-icons nil t))
    (declare-function all-the-icons-faicon 'all-the-icons)
    (declare-function all-the-icons-material 'all-the-icons)
    (declare-function all-the-icons-octicon 'all-the-icons)
    (setq company-box-icons-all-the-icons
          `((Unknown . ,(all-the-icons-material "find_in_page" :height 0.8 :v-adjust -0.15))
            (Text . ,(all-the-icons-faicon "text-width" :height 0.8 :v-adjust -0.02))
            (Method . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
            (Function . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
            (Constructor . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
            (Field . ,(all-the-icons-octicon "tag" :height 0.85 :v-adjust 0 :face 'all-the-icons-lblue))
            (Variable . ,(all-the-icons-octicon "tag" :height 0.85 :v-adjust 0 :face 'all-the-icons-lblue))
            (Class . ,(all-the-icons-material "settings_input_component" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-orange))
            (Interface . ,(all-the-icons-material "share" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
            (Module . ,(all-the-icons-material "view_module" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
            (Property . ,(all-the-icons-faicon "wrench" :height 0.8 :v-adjust -0.02))
            (Unit . ,(all-the-icons-material "settings_system_daydream" :height 0.8 :v-adjust -0.15))
            (Value . ,(all-the-icons-material "format_align_right" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
            (Enum . ,(all-the-icons-material "storage" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-orange))
            (Keyword . ,(all-the-icons-material "filter_center_focus" :height 0.8 :v-adjust -0.15))
            (Snippet . ,(all-the-icons-material "format_align_center" :height 0.8 :v-adjust -0.15))
            (Color . ,(all-the-icons-material "palette" :height 0.8 :v-adjust -0.15))
            (File . ,(all-the-icons-faicon "file-o" :height 0.8 :v-adjust -0.02))
            (Reference . ,(all-the-icons-material "collections_bookmark" :height 0.8 :v-adjust -0.15))
            (Folder . ,(all-the-icons-faicon "folder-open" :height 0.8 :v-adjust -0.02))
            (EnumMember . ,(all-the-icons-material "format_align_right" :height 0.8 :v-adjust -0.15))
            (Constant . ,(all-the-icons-faicon "square-o" :height 0.8 :v-adjust -0.1))
            (Struct . ,(all-the-icons-material "settings_input_component" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-orange))
            (Event . ,(all-the-icons-octicon "zap" :height 0.8 :v-adjust 0 :face 'all-the-icons-orange))
            (Operator . ,(all-the-icons-material "control_point" :height 0.8 :v-adjust -0.15))
            (TypeParameter . ,(all-the-icons-faicon "arrows" :height 0.8 :v-adjust -0.02))
            (Template . ,(all-the-icons-material "format_align_left" :height 0.8 :v-adjust -0.15)))
          company-box-icons-alist 'company-box-icons-all-the-icons)))

(use-package anzu
  :if (window-system)
  :demand t
  :config
  (global-anzu-mode 1)
  :bind (("C-M-%" . anzu-query-replace-regexp)
         ("M-%" . anzu-query-replace)))

(use-package expand-region
  :bind ("M-#" . er/expand-region))

(use-package rg
  :bind
  ("M-g d" . rg))

(use-package wgrep)

(use-package deadgrep
  :bind ("M-g D" . deadgrep))

(use-package xwwp
  :ensure nil
  :load-path "./xwwp"
  :commands (xwwp))

(use-package xwwp-ace
  :ensure nil
  :after xwwp
  :load-path "./xwwp"
  :bind (:map xwidget-webkit-mode-map
              ("t" . xwwp-ace-toggle)))

(use-package xwwp-follow-link
  :ensure nil
  :after xwwp
  :load-path "./xwwp"
  :init
  (setq xwwp-follow-link-completion-system 'default)
  :commands (xwwp)
  :bind (:map xwidget-webkit-mode-map
              ("v" . xwwp-follow-link)))

(use-package avy
  :init (setq avy-case-fold-search nil)
  :bind
  ("C-c M-d" . avy-goto-char-in-line)
  ("C-c M-c" . avy-goto-word-1))

(use-package ace-window
  :bind ( "C-x o" . ace-window)
  :config
  (setq aw-scope 'frame)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package tramp
  :ensure nil
  :init (setq tramp-verbose 6)
  :config
  (setq tramp-password-prompt-regexp
        (concat
         "^.*"
         (regexp-opt
          '("passphrase" "Passphrase"
            ;; English
            "password" "Verification code"
            ;; Deutsch
            "passwort" "Passwort"
            ;; Français
            "mot de passe" "Mot de passe")
          t)
         ".*:\0? *"))
  (setq tramp-default-method "ssh")
  (add-to-list 'tramp-restricted-shell-hosts-alist
               "\\bastion\\'")
  (add-to-list 'tramp-default-proxies-alist
               '("bob$" nil "/sshx:bastion:"))
  (setq remote-file-name-inhibit-cache 3600
        tramp-completion-reread-directory-timeout nil
        vc-ignore-dir-regexp (format "%s\\|%s"
                                     vc-ignore-dir-regexp
                                     tramp-file-name-regexp))
  (setq tramp-histfile-override t)
  ;; Save backup files locally
  ;; from https://stackoverflow.com/a/47021266
  (add-to-list 'backup-directory-alist
               (cons tramp-file-name-regexp "/tmp/emacs-backup/")))

(use-package docker)
(use-package docker-tramp)

(use-package elfeed
  :init
  (setq elfeed-feeds
        '(
          ;; ("https://www.reddit.com/r/listentothis/.rss" music reddit)
          ("https://www.reddit.com/r/emacs/.rss" programming emacs reddit)
          ("http://notarbut.co/feed/podcast" podcast)
          ("https://blog.rust-lang.org/feed.xml" programming rust)
          ;; ("https://www.reddit.com/r/rust/.rss" programming rust reddit)
          ;; ("https://www.reddit.com/r/Clojure/.rss" programming clojure reddit)
          ("https://danluu.com/atom.xml" programming blog)
          ("https://feed.podbean.com/geekonomy/feed.xml" podcast)
          ("https://protesilaos.com/master.xml" programming blog)
          ))
  :bind ("C-c w" . elfeed))
(provide 'navigation)
