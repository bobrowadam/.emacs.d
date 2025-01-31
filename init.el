(setenv "LIBRARY_PATH"
	(string-join
	 '("/opt/homebrew/opt/libgccjit/lib/gcc/14/"
	   "/opt/homebrew/opt/gcc/lib/gcc/13/gcc/aarch64-apple-darwin22/13")
	 ":"))
(setq debug-on-error nil)
(setq lexical-binding t)

(setq package-enable-at-startup nil)

(setq initial-buffer-choice t)
(setq garbage-collection-messages t)

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("gnu-devel" . "https://elpa.gnu.org/devel/")))

(setq package-native-compile t)
(setq use-package-always-defer t)
(setq use-package-always-ensure t)
(setq use-package-verbose t)
(setq use-package-debug t)
(setq use-package-compute-statistics t)
;; use-package-enable-imenu-support must be
;; set before requiring use-package.
(setq use-package-enable-imenu-support t)

(use-package startup
  :demand t
  :load-path "./modules")

(use-package basic-settings
  :after startup
  :load-path "./modules"
  :demand t)

(use-package appearance
  :after startup
  :load-path "./modules"
  :demand t)

(use-package navigation
  :after startup
  :demand t
  :load-path "./modules")

(use-package completions
  :after startup
  :demand t
  :load-path "./modules")

(use-package files-and-buffers
  :after startup
  :demand t
  :load-path "./modules")

(use-package prog
  :after (startup completions)
  :demand t
  :load-path "./modules")

(use-package setup-tree-sitter
  :ensure nil
  :demand t
  :load-path "./modules")

(use-package setup-magit
  :after startup
  :demand t
  :load-path "./modules")

(use-package setup-org
  :after startup
  :demand t
  :load-path "./modules"
  :bind
  ("C-c a" . org-agenda)
  ("C-c c" . org-capture)
  ("C-c s a" . run-cl-asana)
  ("C-c s c" . run-calendar-sync))

(use-package setup-shell
  :demand t
  :after startup
  :load-path "./modules")

(use-package setup-llm
  :demand t
  :after startup
  :load-path "./modules")

(use-package sicp)

(use-package pcsv
  :demand t)

(use-package string-inflection)

(use-package khoj
  :disabled t
  :ensure t
  :pin melpa-stable
  ;; :custom
  ;; (khoj-server-python-command "python3.11")
  ;; (khoj-server-command "khoj")
  :config (setq khoj-org-directories `(,(s-chop-suffix "/" org-directory))
                khoj-openai-api-key (getenv "OPEN_AP_API_KEY")))

(use-package proced-narrow
  :demand t
  :hook (proced-mode . proced-narrow-mode)
  :bind (:map proced-mode-map ("N" . 'proced-narrow)))

(use-package xwwp
  :bind (:map xwidget-webkit-mode-map
              ("C-x C--" . xwidget-webkit-zoom-out)
              ("C-x C-=" . xwidget-webkit-zoom-in)
              ("f" . xwwp-follow-link)))

(use-package uuid
  :commands (uuid-create uuid-string))

(use-package impostman)

(use-package npm-utils
  :commands (bob/update-node-modules-if-needed-sync is-typescript-project)
  :ensure nil)

(use-package grain-utils
  :commands (grain/run-service)
  :ensure nil)

(use-package calc
  :ensure nil
  :bind
  (:map calc-mode-map ("M-i" . casual-calc-tmenu)))

(use-package casual)

(use-package pomm
  :custom
  (pomm-audio-enabled t)
  (pomm-audio-tick-enabled nil)
  :commands (pomm pomm-third-time))

(use-package markdown-mode
  :custom
  (markdown-enable-highlighting-syntax t)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)
         ("\\.mess\\'" . markdown-mode))
  :init (setq markdown-command "pandoc"))

(use-package multiple-cursors
  :bind
  ("C-S-c C-c" . mc/edit-lines)
  ("C-S-c C->" . mc/mark-next-like-this)
  ("C-S-c C-<" . mc/mark-previous-like-this)
  ("C-S-c C-." . mc/mark-all-like-this))

(use-package slack
  :bind (("C-c S m" . slack-im-select)
         ("C-c S K" . slack-stop)
         ("C-c S c" . slack-select-rooms)
         ("C-c S u" . slack-select-unread-rooms)
         ("C-c S U" . slack-user-select)
         ("C-c S s" . slack-search-from-messages)
         ("C-c S J" . slack-jump-to-browser)
         ("C-c S j" . slack-jump-to-app)
         ("C-c S e" . slack-insert-emoji)
         ("C-c S E" . slack-message-edit)
         ("C-c S r" . slack-message-add-reaction)
         ("C-c S t" . slack-thread-show-or-create)
         ("C-c S g" . slack-message-redisplay)
         ("C-c S G" . slack-conversations-list-update-quick)
         ("C-c S q" . slack-quote-and-reply)
         ("C-c S Q" . slack-quote-and-reply-with-link)
         (:map slack-mode-map
               (("@" . slack-message-embed-mention)
                ("#" . slack-message-embed-channel)))
         (:map slack-thread-message-buffer-mode-map
               (("C-c '" . slack-message-write-another-buffer)
                ("@" . slack-message-embed-mention)
                ("#" . slack-message-embed-channel)))
         (:map slack-message-buffer-mode-map
               (("C-c '" . slack-message-write-another-buffer)))
         (:map slack-message-compose-buffer-mode-map
               (("C-c '" . slack-message-send-from-buffer))))

  :custom
  (slack-enable-global-mode-string nil)
  (slack-user-active-string "🞄")
  (slack-buffer-emojify nil)
  (slack-buffer-create-on-notify nil)
  ;; (slack-message-custom-notifier #'bob/slack-sound-notifier)
  ;; (slack-extra-subscribed-channels (mapcar 'intern (list "team-123")))
  :config
  (cl-destructuring-bind 
      (cookie token) (mapcar (lambda (entry)
                               (funcall (plist-get entry :secret)))
                             (auth-source-search :host "slack.io" :max 4))
    (slack-register-team
     :name "grainfinance"
     :token token
     :cookie cookie
     :full-and-display-names t
     :default t
     :subscribed-channels nil)))

(use-package alert
  :commands (alert)
  :init
  :config
  ;; (alert-define-style 'sound
  ;;                     :title "Play a sound for alert"
  ;;                     :notifier (lambda (info)
  ;; (start-process "alert-sound" nil "afplay" (format "%stick.wav" (expand-file-name user-emacs-directory)))))
  (setq alert-default-style 'notifier))

(use-package eradio
  :init
  (setq eradio-channels '(("def con - soma fm" . "https://somafm.com/defcon256.pls")          ;; electronica with defcon-speaker bumpers
                          ("metal - soma fm"   . "https://somafm.com/metal130.pls")           ;; \m/
                          ("cyberia - lainon"  . "https://lainon.life/radio/cyberia.ogg.m3u") ;; cyberpunk-esque electronica
                          ("cafe - lainon"     . "https://lainon.life/radio/cafe.ogg.m3u")    ;; boring ambient, but with lain
                          ("99FM"     . "https://eco01.livecdn.biz/ecolive/99fm_aac/icecast.audio")
                          ("103FM"     . "https://cdn.cybercdn.live/103FM/Live/icecast.audio")
                          ("Kan Bet" . "https://25703.live.streamtheworld.com/KAN_BET.mp3?dist=rlive")
                          ("glglz" . "https://glzwizzlv.bynetcdn.com/glglz_mp3")
                          ("glz" . "https://glzwizzlv.bynetcdn.com/glz_mp3?awCollectionId=misc&awEpisodeId=glz"))))

(put 'dired-find-alternate-file 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'list-timers 'disabled nil)
(put 'magit-clean 'disabled nil)
(put 'magit-edit-line-commit 'disabled nil)
