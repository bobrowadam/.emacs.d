(setq package-enable-at-startup nil)
(setq user-emacs-directory "/Users/bob/.emacs.d/")
(setenv "PATH" "/Users/bob/.opam/default/bin:/Users/bob/.codeium/windsurf/bin:/Users/bob/.bun/bin:/Users/bob/Library/pnpm:/Users/bob/.local/state/fnm_multishells/91475_1743069517342/bin:/opt/local/bin:/opt/local/sbin:/usr/local/bin:/System/Cryptexes/App/usr/bin:/usr/bin:/bin:/usr/sbin:/sbin:/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/local/bin:/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/bin:/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/appleinternal/bin:/Library/TeX/texbin:/usr/local/go/bin:/opt/podman/bin:/opt/homebrew/opt/gcc/bin:/Users/bob/.pyenv/shims:/usr/local/zig:/opt/homebrew/opt/gcc:/Users/bob/.cabal/bin:/Users/bob/.ghcup/bin:/opt/homebrew/bin:/Users/bob/go/bin:/Users/bob/.local/bin:/Library/Frameworks/Python.framework/Versions/3.11/bin:/Users/bob/.cargo/bin:/Users/bob/bin:/Users/bob/.local/lib/aws/bin/aws:/usr/local/opt/libxml2/bin:/Users/bob/.rvm/bin:/usr/local/sbin:/Users/bob/Library/Application Support/Coursier/bin:/usr/local/opt/llvm/bin:/Users/bob/.roswell/bin:/Users/bob/.qlot/bin:/Users/bob/.rustup/toolchains/stable-x86_64-apple-darwin/bin:/Users/bob/cmdline-tools/bin:/Users/bob/Library/Application Support/fnm:/Applications/iTerm.app/Contents/Resources/utilities:/Users/bob/environments/default_env/bin:/opt/homebrew/opt/libpq/bin:/opt/homebrew/opt/llvm/bin")
(setq exec-path (split-string (getenv "PATH") path-separator))

;; Match theme color early on (smoother transition).
;; Theme loaded in features/ui.el.
(add-to-list 'default-frame-alist '(background-color . "#212121"))

;; https://github.com/hlissner/doom-emacs/blob/58af4aef56469f3f495129b4e7d947553f420fca/core/core.el#L200
(unless (daemonp)
  (advice-add #'display-startup-echo-area-message :override #'ignore))

;; https://github.com/hlissner/doom-emacs/blob/58af4aef56469f3f495129b4e7d947553f420fca/core/core.el#L323
(setq frame-inhibit-implied-resize t)

;; https://github.com/hlissner/doom-emacs/blob/58af4aef56469f3f495129b4e7d947553f420fca/core/core.el#L331
(setq inhibit-compacting-font-caches t)

;; https://github.com/hlissner/doom-emacs/blob/58af4aef56469f3f495129b4e7d947553f420fca/core/core.el#L205
(setq idle-update-delay 1.0)

;; Don't want a mode line while loading init.
(setq mode-line-format nil)

;; No scrollbar by default.
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; No nenubar by default.
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))

;; No toolbar by default.
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; No tooltip by default.
(when (fboundp 'tooltip-mode)
  (tooltip-mode -1))

;; No Alarms by default.
(setq ring-bell-function 'ignore)

;; (setq initial-scratch-message nil)
(setq inhibit-startup-screen t)

(setq initial-major-mode 'fundamental-mode)

(defface init-title
  '((t :inherit info-title-3 :height 300))
  "A face For the initial Emacs title.")

(setq gc-cons-percentage-before-init gc-cons-percentage)
(setq gc-cons-threshold-before-init gc-cons-threshold)
(setq gc-cons-percentage 0.6)
(setq gc-cons-threshold (* gc-cons-threshold 1000))
(add-hook 'emacs-startup-hook
          (lambda ()
            (progn (setq gc-cons-percentage gc-cons-percentage-before-init)
                   (setq gc-cons-threshold gc-cons-threshold-before-init))))

(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(add-to-list 'load-path (concat user-emacs-directory "modules"))
