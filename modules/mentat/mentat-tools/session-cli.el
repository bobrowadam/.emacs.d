;;; session-cli.el --- Bash access to Mentat sessions -*- lexical-binding: t; -*-

;;; Commentary:
;; Reuse the session-management skill and Mentat's async callback transport.
;; This adapter does not register model tools or enable the Emacs extension.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'mentat)
(require 'mentat-emacs)
(require 'mentat-elisp-library)
(require 'mentat-profile)
(require 'mentat-registry)
(require 'mentat-skills)
(require 'mentat-buffer)
(require 'mentat-ui)

(declare-function mentat-session-manager--find-entry "session-manager" (session-id))

(defconst bob/mentat-session-cli-operations
  '(("create" . "mentat-session-create")
    ("list" . "mentat-session-list")
    ("search" . "mentat-session-search")
    ("read" . "mentat-session-read")
    ("search-history" . "mentat-session-search-history")
    ("send" . "mentat-session-send")
    ("reload" . "mentat-session-reload")
    ("close" . "mentat-session-close")
    ("kill" . "mentat-session-kill"))
  "Session operations accepted by the shell CLI.")

(defun bob/mentat-session-cli-starter (request-json)
  "Return a callback starter for the session operation in REQUEST-JSON.
Create uses the requested named profile.  Reload accepts a stable session ID.
Results are JSON text delivered through Mentat's existing callback transport."
  (unless (fboundp 'mentat-session-create)
    (let* ((file (expand-file-name "modules/mentat/skills/session-management.org"
                                   user-emacs-directory))
           (mentat-org-skill-directories
            (cons (file-name-directory file) mentat-org-skill-directories)))
      (mentat-org-skill-read file)))
  (let* ((request (json-parse-string request-json :object-type 'alist
                                   :null-object nil :false-object :false))
         (operation (alist-get 'command request))
         (name (cdr (assoc operation bob/mentat-session-cli-operations)))
         (arguments (alist-get 'arguments request))
         (keywords (alist-get 'keywords request))
         profile-directory)
    (unless name (user-error "Unknown session operation: %s" operation))
    (when (equal operation "create")
      (let* ((profile-name (alist-get 'profile request))
             (profile (assoc profile-name mentat-pi-profiles)))
        (unless profile
          (user-error "Unknown Mentat profile: %s" profile-name))
        (setq profile-directory
              (mentat--profile-normalize-directory
               (mentat--profile-directory profile)))
        (unless (and profile-directory (file-directory-p profile-directory))
          (user-error "Mentat profile directory is unavailable: %s" profile-name))))
    (when (equal operation "reload")
      (let* ((entry (mentat-session-manager--find-entry (aref arguments 0)))
             (view (mentat--open-live-view
                    (mentat--registry-entry-session-id entry)))
             (buffer (and view (mentat--buffer-buffer view))))
        (unless (buffer-live-p buffer)
          (user-error "Session is not live; reload requires a live session"))
        (when (get-buffer-window buffer t)
          (user-error "Session is displayed; hide its buffer before reloading"))
        (aset arguments 0 (buffer-name buffer))))
    (let* ((arguments-json (json-encode arguments))
           (keywords-json (json-encode keywords))
           (execution (mentat-elisp-function-execution
                       (gethash (intern name) mentat--elisp-function-registry))))
      (lambda (resolve reject on-cancel)
        (let ((mentat-pi-directory (or profile-directory mentat-pi-directory)))
          (cl-labels
              ((finish (value)
                 (funcall resolve
                          (mentat-result-text
                           (json-encode
                            (if (equal operation "list") (vconcat value) value))))))
            (if (eq execution 'async)
                (funcall (mentat-elisp-call-async-starter
                          name arguments-json keywords-json)
                         #'finish reject on-cancel)
              (finish (mentat-elisp-call-sync
                       name arguments-json keywords-json)))))))))

(provide 'session-cli)
;;; session-cli.el ends here
