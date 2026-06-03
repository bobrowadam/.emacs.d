;; -*- lexical-binding: t; -*-
;;
;; kitty-pi.el — launch/focus the Kitty tab that hosts pi for the current
;; monorepo, plus the three global keybindings that drive pi from Emacs.
;; Everything pi-generic (sending regions, pulse-on-edit, image paste,
;; responses, diagnostics) lives in the local pi-emacs module:
;;   ~/.emacs.d/modules/pi-emacs.el
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

(defun bob/kitty-find-pi-tab (dir &optional include-nested)
  "Find a Kitty tab running pi in DIR, return its active window ID.
When INCLUDE-NESTED is non-nil, also match pi sessions in subdirectories of DIR.
Prefers exact cwd matches, then focused nested matches, then the first nested match."
  (condition-case nil
      (let ((os-windows (json-parse-string (bob/kitten "ls") :array-type 'list))
            (dir (directory-file-name (expand-file-name dir)))
            (focused nil)
            (first-match nil)
            (focused-nested nil)
            (first-nested nil))
        (dolist (os-win os-windows)
          (dolist (tab (gethash "tabs" os-win))
            (dolist (win (gethash "windows" tab))
              (when-let* ((proc (cl-find-if
                                  (lambda (proc)
                                    (member "pi" (append (gethash "cmdline" proc) nil)))
                                  (append (gethash "foreground_processes" win) nil)))
                          (cwd (gethash "cwd" proc))
                          (cwd (directory-file-name (expand-file-name cwd))))
                (let ((exact (string= cwd dir))
                      (nested (and include-nested (file-in-directory-p cwd dir)))
                      (id (number-to-string (gethash "id" win))))
                  (cond
                   (exact
                    (unless first-match (setq first-match id))
                    (when (eq (gethash "is_focused" win) t)
                      (setq focused id)))
                   (nested
                    (unless first-nested (setq first-nested id))
                    (when (eq (gethash "is_focused" win) t)
                      (setq focused-nested id)))))))))
        (or focused first-match focused-nested first-nested))
    (error nil)))

(defun bob/open-pi-in-kitty ()
  "Open or focus Pi coding agent in Kitty for the current project root."
  (interactive)
  (let* ((dir (file-name-as-directory
               (expand-file-name
                (or (when-let* ((project (project-current nil)))
                      (project-root project))
                    (bob/monorepo-root)))))
         (tab-id (gethash dir bob/kitty-pi-tabs)))
    ;; Try hash first, then discover existing tab by cwd+process, then launch new.
    (cond
     ((and tab-id (bob/kitty-tab-exists-p tab-id))
      (bob/kitty-focus-tab tab-id))
     ((when-let* ((found-id (bob/kitty-find-pi-tab dir (derived-mode-p 'magit-status-mode))))
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
;; in the pi-emacs module now (see ~/.emacs.d/modules/pi-emacs.el).  This file
;; is only for things that depend on my personal Kitty + monorepo setup.

;;; Keybindings

(global-set-key (kbd "C-c p p") #'bob/open-pi-in-kitty)
(global-set-key (kbd "C-c p s") #'bob/pi-send-buffer-context)
(global-set-key (kbd "C-c p a") #'pi/ask)

(provide 'kitty-pi)
