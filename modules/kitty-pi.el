;; -*- lexical-binding: t; -*-
;;
;; kitty-pi.el — launch/focus the Kitty tab that hosts pi for the current
;; monorepo, plus the three global keybindings that drive pi from Emacs.
;; Everything pi-generic (sending regions, pulse-on-edit, image paste,
;; responses, diagnostics) lives in the local pi-emacs module:
;;   ~/.emacs.d/modules/pi-emacs.el
;;
;; Assumes `bob/monorepo-root' is defined in init.org.

(require 'cl-lib)
(require 'project)
(require 'subr-x)

(declare-function bob/monorepo-root "init")
(declare-function bob/pi-send-buffer-context "init")
(declare-function pi/ask "pi-emacs")

;;; Kitty helpers

(defun bob/kitty-socket ()
  "Find the Kitty remote control socket."
  (car (file-expand-wildcards "/tmp/kitty-emacs*")))

(defun bob/kitten (&rest args)
  "Run kitten with --to socket and ARGS. Return trimmed stdout."
  (if-let* ((sock (bob/kitty-socket)))
      (string-trim
       (with-output-to-string
         (apply #'call-process "kitten" nil standard-output nil
                "@" "--to" (concat "unix:" sock) args)))
    (error "No Kitty socket found — is Kitty running?")))

(defalias 'pi/kitten #'bob/kitten)

(defun bob/kitty-tab-exists-p (window-id)
  "Return non-nil if a Kitty tab containing WINDOW-ID still exists.
WINDOW-ID may be a string or integer; this is the value returned by
`kitten @ launch'."
  (condition-case nil
      (let* ((numeric-id (if (stringp window-id) (string-to-number window-id) window-id))
             (os-windows (json-parse-string (bob/kitten "ls") :array-type 'list)))
        (cl-some (lambda (os-win)
                   (cl-some (lambda (tab)
                              (cl-some (lambda (win)
                                         (= (gethash "id" win) numeric-id))
                                       (gethash "windows" tab)))
                            (gethash "tabs" os-win)))
                 os-windows))
    (error nil)))

(defun bob/kitty-focus-tab (tab-id)
  "Focus the Kitty tab with TAB-ID."
  (bob/kitten "focus-tab" "--match" (format "id:%s" tab-id))
  (call-process "open" nil nil nil "-a" "kitty"))

;;; Session tracking

(defvar bob/kitty-pi-tabs (make-hash-table :test 'equal)
  "Maps git roots to the last chosen Kitty window ID for live pi sessions.")

(defvar bob/kitty-pi--last-session-per-root (make-hash-table :test 'equal)
  "Maps git roots to the most recently chosen live Pi window ID.")

(defun bob/kitty-pi--normalize-dir (dir)
  "Normalize DIR for stable workspace comparisons."
  (directory-file-name (expand-file-name dir)))

(defun bob/kitty-pi--git-root (&optional dir)
  "Return the git-root workspace for DIR.
Falls back to `bob/monorepo-root' or DIR when no git root is found."
  (let ((dir (bob/kitty-pi--normalize-dir (or dir default-directory))))
    (bob/kitty-pi--normalize-dir
     (or (locate-dominating-file dir ".git")
         (and (fboundp 'bob/monorepo-root)
              (bob/monorepo-root))
         dir))))

(defun bob/kitty-pi--launch-dir ()
  "Return the directory to use when launching a new Pi session."
  (bob/kitty-pi--normalize-dir
   (or (when-let* ((project (project-current nil)))
         (project-root project))
       (and (fboundp 'bob/monorepo-root)
            (bob/monorepo-root))
       default-directory)))

(defconst bob/kitty-pi--socket-name-basename-length 24
  "Maximum basename prefix length used in Pi socket filenames.")

(defconst bob/kitty-pi--socket-name-hash-length 16
  "Hex digest length used in Pi socket filenames.")

(defun bob/kitty-pi--encode-cwd (cwd)
  "Encode CWD to match Pi's hashed session socket naming."
  (let* ((dir (bob/kitty-pi--normalize-dir cwd))
         (base (file-name-nondirectory dir))
         (safe-base (replace-regexp-in-string
                     "^-+\\|-+$" ""
                     (replace-regexp-in-string "[^[:alnum:]._-]+" "-" base)))
         (safe-base (if (string-empty-p safe-base) "cwd" safe-base))
         (safe-base (if (> (length safe-base) bob/kitty-pi--socket-name-basename-length)
                        (substring safe-base 0 bob/kitty-pi--socket-name-basename-length)
                      safe-base))
         (hash (substring (secure-hash 'sha256 dir)
                          0 bob/kitty-pi--socket-name-hash-length)))
    (format "%s-%s" safe-base hash)))

(defun bob/kitty-pi--socket-dir ()
  "Return the directory containing Pi Unix sockets."
  (expand-file-name "sockets" "~/.pi"))

(defun bob/kitty-pi--live-cwd-p (git-root cwd)
  "Return non-nil when CWD has a live Pi socket for GIT-ROOT."
  (let* ((encoded-cwd (bob/kitty-pi--encode-cwd cwd))
         (direct (expand-file-name (concat encoded-cwd ".sock")
                                   (bob/kitty-pi--socket-dir)))
         (indexed (expand-file-name
                   (concat "by-worktree/"
                           (bob/kitty-pi--encode-cwd git-root)
                           "/" encoded-cwd ".sock")
                   (bob/kitty-pi--socket-dir))))
    (or (file-exists-p indexed)
        (file-exists-p direct))))

(defun bob/kitty-pi--process-p (proc)
  "Return non-nil when PROC looks like a Pi foreground process."
  (seq-some (lambda (arg)
              (string= (file-name-nondirectory arg) "pi"))
            (append (gethash "cmdline" proc) nil)))

(defun bob/kitty-pi--foreground-pi-process (win)
  "Return WIN's foreground Pi process, or nil."
  (cl-find-if #'bob/kitty-pi--process-p
              (append (gethash "foreground_processes" win) nil)))

(defun bob/kitty-pi--live-session-records (&optional _target-root)
  "Return live Pi Kitty windows in plists.
Each record has :id, :cwd, :git-root, :focused, :title, and :cmdline keys.
Uses the foreground Pi process cwd, not Kitty's window cwd, so forked sessions
survive stale Kitty cwd values after worktree renames.  A foreground Pi process
is sufficient for Kitty tab switching even if its socket pathname was unlinked."
  (condition-case nil
      (let ((os-windows (json-parse-string (bob/kitten "ls") :array-type 'list))
            sessions)
        (dolist (os-win os-windows)
          (dolist (tab (gethash "tabs" os-win))
            (dolist (win (gethash "windows" tab))
              (when-let* ((proc (bob/kitty-pi--foreground-pi-process win))
                          (cwd (gethash "cwd" proc))
                          (cwd (bob/kitty-pi--normalize-dir cwd))
                          (git-root (bob/kitty-pi--git-root cwd)))
                (push (list :id (number-to-string (gethash "id" win))
                            :cwd cwd
                            :git-root git-root
                            :socket-live (bob/kitty-pi--live-cwd-p git-root cwd)
                            :focused (eq (gethash "is_focused" win) t)
                            :title (or (gethash "title" win) "")
                            :cmdline (string-join (append (gethash "cmdline" proc) nil) " "))
                      sessions)))))
        (nreverse sessions))
    (error nil)))

(defun bob/kitty-pi--session-display (session)
  "Return a human-friendly label for SESSION."
  (let* ((git-root (plist-get session :git-root))
         (cwd (plist-get session :cwd))
         (title (plist-get session :title))
         (focused (plist-get session :focused))
         (last-id (gethash git-root bob/kitty-pi--last-session-per-root))
         (workspace (file-name-nondirectory git-root))
         (relative (if (string= cwd git-root)
                       "."
                     (file-relative-name cwd git-root)))
         (base (if (string= relative ".")
                   workspace
                 (format "%s › %s" workspace relative)))
         (label (if (and title (not (string-empty-p title)))
                    (format "%s — %s" base title)
                  base))
         (markers (delq nil
                        (list (when focused "focused")
                              (when (equal (plist-get session :id) last-id)
                                "last")))))
    (if markers
        (format "%s [%s]" label (string-join markers ", "))
      label)))

(defun bob/kitty-pi--sessions-for-root (git-root)
  "Return live Pi Kitty windows for GIT-ROOT, ordered by priority."
  (let* ((git-root (bob/kitty-pi--git-root git-root))
         (last-id (gethash git-root bob/kitty-pi--last-session-per-root))
         (sessions (cl-remove-if-not
                    (lambda (session)
                      (string= (plist-get session :git-root) git-root))
                    (bob/kitty-pi--live-session-records git-root))))
    (sort sessions
          (lambda (a b)
            (let* ((a-last (equal (plist-get a :id) last-id))
                   (b-last (equal (plist-get b :id) last-id))
                   (a-focused (plist-get a :focused))
                   (b-focused (plist-get b :focused))
                   (a-depth (length (split-string (plist-get a :cwd) "/" t)))
                   (b-depth (length (split-string (plist-get b :cwd) "/" t))))
              (cond
               ((and a-last (not b-last)) t)
               ((and b-last (not a-last)) nil)
               ((and a-focused (not b-focused)) t)
               ((and b-focused (not a-focused)) nil)
               ((/= a-depth b-depth) (> a-depth b-depth))
               (t (string< (plist-get a :cwd)
                           (plist-get b :cwd)))))))))

(defun bob/kitty-pi--remember-session (git-root session-id)
  "Remember SESSION-ID as the last visited live Pi window for GIT-ROOT."
  (puthash git-root session-id bob/kitty-pi-tabs)
  (puthash git-root session-id bob/kitty-pi--last-session-per-root)
  session-id)

;;; Kitty window ID lookup

(defun bob/kitty-pi-window-id (&optional dir)
  "Return a live Kitty window ID for DIR's git root.
Prefers the most recently chosen session, then the focused live session, then
another live session in the same git-root workspace. Returns a string or nil."
  (let* ((git-root (bob/kitty-pi--git-root dir))
         (cached (gethash git-root bob/kitty-pi-tabs)))
    (cond
     ((and cached (bob/kitty-tab-exists-p cached)) cached)
     (t
      (when-let* ((session (car (bob/kitty-pi--sessions-for-root git-root))))
        (bob/kitty-pi--remember-session git-root (plist-get session :id)))))))

;;; Open / focus

(defun bob/kitty-pi--choose-session (git-root)
  "Prompt for one live Pi session in GIT-ROOT and return the matching record."
  (let* ((sessions (bob/kitty-pi--sessions-for-root git-root))
         (choices (mapcar (lambda (session)
                            (cons (bob/kitty-pi--session-display session)
                                  session))
                          sessions))
         (selected
          (cond
           ((null choices) nil)
           ((= (length choices) 1) (caar choices))
           ((fboundp 'consult--read)
            (consult--read (mapcar #'car choices)
                           :prompt (format "Pi session for %s: "
                                           (file-name-nondirectory git-root))
                           :sort nil
                           :require-match t
                           :default (caar choices)))
           (t
            (completing-read (format "Pi session for %s: "
                                     (file-name-nondirectory git-root))
                             (mapcar #'car choices)
                             nil t nil nil (caar choices))))))
    (cdr (assoc selected choices))))

(defun bob/kitty-find-pi-tab (dir)
  "Find the closest live Pi tab for DIR's git root.
Only Pi sessions whose cwd shares the same git root match.  This keeps child
projects from stealing focus when the current buffer is inside a parent or
sibling worktree."
  (let* ((git-root (bob/kitty-pi--git-root dir))
         (sessions (bob/kitty-pi--sessions-for-root git-root)))
    (when-let* ((session (car sessions)))
      (plist-get session :id))))

(defun bob/open-pi-in-kitty ()
  "Open or focus Pi coding agent for the current git-root workspace."
  (interactive)
  (let* ((workspace-root (bob/kitty-pi--git-root default-directory))
         (launch-dir (bob/kitty-pi--launch-dir))
         (session (bob/kitty-pi--choose-session workspace-root))
         (tab-id (and session (plist-get session :id))))
    (cond
     (tab-id
      (bob/kitty-pi--remember-session workspace-root tab-id)
      (bob/kitty-focus-tab tab-id))
     (t
      (let* ((args (list "launch" "--type=tab"
                         "--cwd" launch-dir
                         "/bin/zsh" "-li" "-c" "pi; exec /bin/zsh -li"))
             (out (apply #'bob/kitten args))
             (win-id (when (string-match "\\([0-9]+\\)" out)
                       (match-string 1 out)))
             (dark-p (string= "Dark"
                              (string-trim
                               (shell-command-to-string
                                "defaults read -g AppleInterfaceStyle 2>/dev/null || echo Light"))))
             (theme-file (expand-file-name
                          (if dark-p "kanagawa.conf" "kanagawa-light.conf")
                          "~/.config/kitty")))
        (when win-id
          (bob/kitty-pi--remember-session workspace-root win-id)
          (bob/kitten "set-colors" "--match" (format "id:%s" win-id) theme-file))
        (call-process "open" nil nil nil "-a" "kitty"))))))

;; Sending regions to pi, pulse-on-edit, and clipboard-image paste all live
;; in the pi-emacs module now (see ~/.emacs.d/modules/pi-emacs.el).  This file
;; is only for things that depend on my personal Kitty + monorepo setup.

;;; Keybindings

(global-set-key (kbd "C-c p p") #'bob/open-pi-in-kitty)
(global-set-key (kbd "C-c p s") #'bob/pi-send-buffer-context)
(global-set-key (kbd "C-c p a") #'pi/ask)

(provide 'kitty-pi)
