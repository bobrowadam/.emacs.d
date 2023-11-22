(use-package consult
  :demand t
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

(use-package iserach
  :ensure nil
  :init
  (setq isearch-lazy-count t)
  (setq search-whitespace-regexp ".*?"))

(use-package occur
  :ensure nil
  :bind ("C-c o" . occur))

(use-package consult-project-extra
  :bind
  (("C-x p f" . consult-project-extra-find)
   ("C-x p o" . consult-project-extra-find-other-window)))

(defun browse-current-project ()
  (interactive)
  (browse-riseup-git-project (project-name (project-current))))

(defun project-list-file-buffers ()
  (interactive)
  (project-list-buffers 1)
  (pop-to-buffer "*Buffer List*"))

(use-package project
  :ensure nil
  :bind
  (("C-x p w" . project-switch-to-open-project)
   ;; ("C-x p s" . bob/project-vterm)
   ("C-x p s" . eat-project)
   ("C-x p m"  . magit-project-status)
   ("C-x p C-m"  . project-dired)
   ("C-x p i" . #'project-list-file-buffers))
  :init
  (setq project-switch-commands
        '((consult-project-extra-find "Find file")
          (project-dired "Find directory")
          (eat-project "Eat" "s")
          (magit-project-status "Magit" "g")
          (consult-ripgrep "Grep" "r")
          (project-ibuffer "IBuffers" "b")
          (project-list-file-buffers "List Buffers" "i")
          (browse-current-project "Browse" "B")))
  (unless (project-known-project-roots)
    (message "No project file found, indexing projects")
    (progn
      (project-remember-projects-under "~/source/" t)
      (project-remember-projects-under "~/source/services" t)
      (project-remember-projects-under "~/source/common-lisp/" t))))

(use-package savehist
  :ensure nil
  :init
  (setq savehist-file (locate-user-emacs-file "savehist"))
  (setq history-length 10000)
  (setq history-delete-duplicates t)
  (setq savehist-save-minibuffer-history t)
  (savehist-mode))

(use-package recentf-mode
  :ensure nil
  :init
  (recentf-mode 1))

(use-package orderless
  :init
  (setq completion-ignore-case t)
  (setq orderless-component-separator " +")
  (setq orderless-matching-styles
        '(orderless-literal orderless-flex))
  (setq completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion))))
  (setq completion-styles '(basic orderless)))


(use-package vertico
  :demand t
  :init
  (setq vertico-scroll-margin 0)
  (setq vertico-count 6)
  (setq vertico-resize nil)
  (setq vertico-cycle t)
  :hook
  (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :config
  (vertico-mode 1))

(use-package marginalia
  :after vertico
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


;; A few more useful configurations...

(use-package emacs
  :ensure nil
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

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
  :disabled t
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

(use-package avy
  :init (setq avy-case-fold-search nil)
  :bind
  ("C-c M-d" . avy-goto-char-in-line)
  ("C-c M-c" . avy-goto-word-1))

(use-package ace-window
  :bind
  ( "C-x o" . ace-window)
  ( "M-o" . ace-window)
  :config
  (setq aw-scope 'frame)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package tramp
  :ensure nil
  :init (setq tramp-verbose 1)
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
  (setq remote-file-name-inhibit-locks t
        remote-file-name-inhibit-cache 3600
        tramp-completion-reread-directory-timeout nil
        vc-ignore-dir-regexp (format "\\(%s\\)\\|\\(%s\\)"
                                     vc-ignore-dir-regexp
                                     tramp-file-name-regexp))
  (setq tramp-histfile-override t)
  (add-to-list 'backup-directory-alist
               (cons tramp-file-name-regexp "/tmp/emacs-backup/")))

(use-package docker)
(use-package tramp-container :ensure nil)

(use-package elfeed
  :init
  (setq elfeed-feeds
        '(("https://www.haaretz.co.il/srv/rss---feedly" news politics haaretz)
          ("https://feeds.resonaterecordings.com/software-unscripted" programming)
          ("https://feeds.buzzsprout.com/1887966.rss" politics)
          ("https://www.omnycontent.com/d/playlist/2ee97a4e-8795-4260-9648-accf00a38c6a/5e87674c-9ff9-4a34-87ea-adb8010d232e/dae4c5e9-ceed-4d7d-a7c5-adb900952d20/podcast.rss" comedy politics)
          ("https://feeds.transistor.fm/thoughts-on-functional-programming-podcast-by-eric-normand" programming lisp)
          ;; ("https://www.reddit.com/r/listentothis/.rss" music reddit)
          ("https://www.reddit.com/r/emacs/.rss" programming emacs reddit)
          ("https://www.omnycontent.com/d/playlist/2ee97a4e-8795-4260-9648-accf00a38c6a/661e2338-316e-4a0c-a2ab-ace100c4f08b/1c1fe6c7-ca02-4358-a7e3-ace100c4f0a3/podcast.rss" podcast politics)
          ("http://notarbut.co/feed/podcast" podcast)
          ("https://blog.rust-lang.org/feed.xml" programming rust)
          ;; ("https://www.reddit.com/r/rust/.rss" programming rust reddit)
          ;; ("https://www.reddit.com/r/Clojure/.rss" programming clojure reddit)
          ("https://danluu.com/atom.xml" programming blog)
          ("https://feed.podbean.com/geekonomy/feed.xml" podcast)
          ("https://protesilaos.com/master.xml" programming blog emacs)))
  :bind ("C-c w" . elfeed))

(use-package elfeed-webkit
  :disabled t
  :after elfeed
  :demand t
  :config (elfeed-webkit-enable))

(use-package control-mode
  :disabled t
  :init
  (control-mode-default-setup))

;; This mode is similar to "control-mode" but more opinionated
(use-package god-mode
  :disabled t
  :init
  (defun my-god-mode-update-mode-line ()
    (cond
     (god-local-mode
      (set-face-attribute 'mode-line nil
                          :foreground "#604000"
                          :background "#fff29a")
      (set-face-attribute 'mode-line-inactive nil
                          :foreground "#3f3000"
                          :background "#fff3da"))
     (t
      (set-face-attribute 'mode-line nil
                          :foreground "#0a0a0a"
                          :background "#d7d7d7")
      (set-face-attribute 'mode-line-inactive nil
                          :foreground "#404148"
                          :background "#efefef"))))
  :bind ("C-z" . god-mode-all)
  :hook (post-command-hook . #'my-god-mode-update-mode-line))

;;;###autoload
(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

;;;###autoload
(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(bind-key "C-S-p" 'move-line-up)
(bind-key "C-S-n" 'move-line-down)

(defun bob/make-frame-a-list ()
  (cl-remove-if (lambda (frame)
                  (eq (car frame)
                      (frame-parameter nil 'name)))
                (make-frame-names-alist)))

(defvar bob/frame-name-history nil)
(defun bob/select-frame-by-name (name)
  "Select the frame whose name is NAME and raise it.
Frames on the current terminal are checked first.
If there is no frame by that name, signal an error."
  (interactive
   (let* ((frame-names-alist (bob/make-frame-a-list))
           (default (car (car frame-names-alist)))
           (input (completing-read
                   (format-prompt "Select Frame" default)
                   frame-names-alist nil t nil 'bob/frame-name-history)))
     (if (= (length input) 0)
         (list default)
       (list input))))
  (select-frame-set-input-focus
   ;; Prefer frames on the current display.
   (or (cdr (assoc name (bob/make-frame-a-list)))
       (catch 'done
         (dolist (frame (frame-list))
           (when (equal (frame-parameter frame 'name) name)
             (throw 'done frame))))
       (error "There is no frame named `%s'" name))))

(use-package beframe
  :load-path "~/source/beframe/"
  :demand t
  :after consult
  :config
  (defvar consult-buffer-sources)
  (declare-function consult--buffer-state "consult")
  (defface beframe-buffer
    '((t :inherit font-lock-string-face))
    "Face for `consult' framed buffers.")
  (defvar beframe-consult-source
    `( :name     "Frame-specific buffers (current frame)"
       :narrow   ?F
       :category buffer
       :face     beframe-buffer
       :history  beframe-history
       :items    ,(lambda () (remove (buffer-name) (beframe-buffer-names)))
       :action   ,#'switch-to-buffer
       :state    ,#'consult--buffer-state))

  (add-to-list 'consult-buffer-sources 'beframe-consult-source)
  (define-key global-map (kbd "C-c b") beframe-prefix-map)
  ;; (setq beframe-functions-in-frames '(project-prompt-project-dir))
  (setq beframe-functions-in-frames '())
  (setq beframe-global-buffers '("*Messages*"))
  (beframe-mode 1)
  :bind

  (:map beframe-prefix-map
        ("r" . beframe-rename-frame)
        ("o" . select-frame-by-name)))

(provide 'navigation)
