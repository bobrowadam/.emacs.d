;;; session-cli.el --- Bash access to Mentat sessions -*- lexical-binding: t; -*-

;;; Commentary:
;; Adapt the private CLI to native session operations and async transport.
;; History is read by the CLI using Mentat's shared TypeScript reader.

;;; Code:

(require 'json)
(require 'mentat)
(require 'mentat-emacs)
(require 'mentat-elisp-library)
(require 'mentat-profile)
(require 'mentat-sessions)

(defun bob/mentat-session-cli-starter (request-json)
  "Return a callback starter for the native request in REQUEST-JSON.
Create uses the requested named profile.  Results use native JSON field names."
  (let* ((request (json-parse-string request-json :object-type 'alist
                                   :null-object nil :false-object :json-false))
         (operation (alist-get 'operation request))
         profile-directory)
    (unless (member operation '("create" "list" "search" "transcript"
                               "send" "reload" "close" "kill"))
      (user-error "Unknown session operation: %s" operation))
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
      (let* ((view (mentat--open-live-view (alist-get 'session_id request)))
             (buffer (and view (mentat--buffer-buffer view))))
        (unless (buffer-live-p buffer)
          (user-error "Session is not live; reload requires a live session"))
        (when (get-buffer-window buffer t)
          (user-error "Session is displayed; hide its buffer before reloading"))))
    (lambda (resolve reject on-cancel)
      (let ((mentat-pi-directory (or profile-directory mentat-pi-directory)))
        (funcall
         (mentat-sessions-starter request-json)
         (lambda (value)
           (if (equal operation "transcript")
               (let ((result (json-parse-string value :object-type 'alist)))
                 (funcall resolve
                          (mentat-result-text
                           (json-encode
                            (append result
                                   `((extension_directory
                                      . ,(expand-file-name
                                          "pi-extensions"
                                          (file-name-directory
                                           (locate-library "mentat"))))))))))
             (funcall resolve (mentat-result-text value))))
         reject on-cancel)))))

(provide 'session-cli)
;;; session-cli.el ends here
