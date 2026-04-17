;; -*- lexical-binding: t; -*-
;;
;; pi.el — Emacs integration for the Pi coding agent running in Kitty.
;;
;; Assumes the following helpers are defined in init.org:
;;   bob/monorepo-root, bob/kitten, bob/kitty-socket,
;;   bob/kitty-tab-exists-p, bob/kitty-focus-tab

(declare-function bob/monorepo-root "init")
(declare-function bob/kitten "init")
(declare-function bob/kitty-socket "init")
(declare-function bob/kitty-tab-exists-p "init")
(declare-function bob/kitty-focus-tab "init")

;;; Session tracking

(defvar bob/kitty-pi-tabs (make-hash-table :test 'equal)
  "Maps monorepo root dirs to Kitty window IDs for pi sessions.")

;;; Kitty window ID lookup

(defun bob/kitty-pi-window-id (&optional dir)
  "Return the Kitty window ID for the pi session in DIR (or current project).
Searches the hash table first, then queries Kitty.  Returns a string or nil."
  (let* ((dir (expand-file-name (or dir (bob/monorepo-root))))
         (cached (gethash dir bob/kitty-pi-tabs)))
    (if (and cached (bob/kitty-tab-exists-p cached))
        cached
      (when-let* ((found (bob/kitty-find-pi-tab dir)))
        (puthash dir found bob/kitty-pi-tabs)
        found))))

;;; Open / focus

(defun bob/kitty-find-pi-tab (dir)
  "Find a Kitty tab running pi in DIR, return its active window ID
as string, or nil.  Prefers the focused window when multiple match."
  (condition-case nil
      (let ((os-windows (json-parse-string (bob/kitten "ls") :array-type 'list))
            (focused nil)
            (first-match nil))
        (dolist (os-win os-windows)
          (dolist (tab (gethash "tabs" os-win))
            (dolist (win (gethash "windows" tab))
              (when (cl-some (lambda (proc)
                               (and (string= (gethash "cwd" proc) (directory-file-name dir))
                                    (member "pi" (append (gethash "cmdline" proc) nil))))
                             (append (gethash "foreground_processes" win) nil))
                (let ((id (number-to-string (gethash "id" win))))
                  (unless first-match (setq first-match id))
                  (when (eq (gethash "is_focused" win) t)
                    (setq focused id)))))))
        (or focused first-match))
    (error nil)))

(defun bob/open-pi-in-kitty ()
  "Open or focus Pi coding agent in Kitty for the current project root."
  (interactive)
  (let* ((dir (expand-file-name (bob/monorepo-root)))
         (tab-id (gethash dir bob/kitty-pi-tabs)))
    ;; Try hash first, then discover existing tab by cwd+process, then launch new.
    (cond
     ((and tab-id (bob/kitty-tab-exists-p tab-id))
      (bob/kitty-focus-tab tab-id))
     ((when-let* ((found-id (bob/kitty-find-pi-tab dir)))
        (puthash dir found-id bob/kitty-pi-tabs)
        (bob/kitty-focus-tab found-id)
        t))
     (t
      (remhash dir bob/kitty-pi-tabs)
      (let* ((args (list "launch" "--type=tab"
                         "--cwd" dir
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
          (puthash dir win-id bob/kitty-pi-tabs)
          (bob/kitten "set-colors" "--match" (format "id:%s" win-id) theme-file))
        (call-process "open" nil nil nil "-a" "kitty"))))))

;; Sending regions to pi, pulse-on-edit, and clipboard-image paste all live
;; in the pi-coding-agent emacs extension now (see
;; ~/.pi/agent/extensions/emacs/elisp/pi-emacs.el).  This file is only for
;; things that depend on my personal Kitty + monorepo setup.

;;; Keybindings

(global-set-key (kbd "C-c p p") #'bob/open-pi-in-kitty)
(global-set-key (kbd "C-c p s") #'pi/send-buffer-context)
(global-set-key (kbd "C-c p a") #'pi/ask)

(provide 'pi-helpers)
