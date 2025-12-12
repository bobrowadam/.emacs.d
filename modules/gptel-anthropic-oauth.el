;;; gptel-anthropic-oauth.el --- OAuth authentication for Anthropic/Claude in gptel  -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: Bob
;; Keywords: convenience, anthropic, oauth
;; Package-Requires: ((emacs "27.1") (gptel "0.9.0"))

;;; Commentary:

;; This module adds OAuth authentication support for Anthropic/Claude
;; in gptel, allowing browser-based authentication instead of manual
;; API key management.
;;
;; Based on the OAuth implementation from ECA (Editor Code Assistant):
;; https://github.com/cljoly/eca
;;
;; Usage:
;;   (require 'gptel-anthropic-oauth)
;;   M-x gptel-anthropic-login
;;
;; Two authentication modes are supported:
;; - console: Creates a static API key (standard Claude console)
;; - max: Uses refresh tokens with auto-renewal (Claude Pro/Max)
;;
;; Tokens are stored in ~/.emacs.d/.cache/anthropic-oauth/ and
;; automatically restored across Emacs sessions.

;;; Code:

(require 'gptel)
(require 'gptel-anthropic)
(require 'gptel-openai)  ; For gptel--json-encode macro
(require 'url)
(require 'json)

;;; OAuth Constants

(defconst gptel-anthropic-oauth--client-id "9d1c250a-e61b-44d9-88ed-5944d1962f5e"
  "Anthropic OAuth client ID.")

(defconst gptel-anthropic-oauth--redirect-uri "https://console.anthropic.com/oauth/code/callback"
  "OAuth redirect URI.")

(defconst gptel-anthropic-oauth--scope "org:create_api_key user:profile user:inference"
  "OAuth scopes to request.")

;;; Configuration

(defcustom gptel-anthropic-oauth-tokens-dir
  (expand-file-name ".cache/anthropic-oauth/" user-emacs-directory)
  "Directory where Anthropic OAuth tokens are stored."
  :type 'directory
  :group 'gptel)

(defvar gptel-anthropic-oauth-token-file
  (expand-file-name "tokens.el" gptel-anthropic-oauth-tokens-dir)
  "File where Anthropic OAuth tokens are cached.")

;;; Backend State Storage

(defvar gptel-anthropic-oauth--backend-state (make-hash-table :test 'eq)
  "Hash table mapping backends to their OAuth state.")

(defun gptel-anthropic-oauth--get-backend-key (backend)
  "Get a unique key for BACKEND.
Uses the backend object itself as key since hash tables support :test 'eq."
  backend)

(defun gptel-anthropic-oauth--set-tokens (backend tokens)
  "Store TOKENS for BACKEND."
  (let ((key (gptel-anthropic-oauth--get-backend-key backend))
        (state (gethash (gptel-anthropic-oauth--get-backend-key backend)
                       gptel-anthropic-oauth--backend-state
                       '())))
    (puthash key (plist-put state :tokens tokens)
            gptel-anthropic-oauth--backend-state)))

(defun gptel-anthropic-oauth--get-tokens (backend)
  "Get tokens for BACKEND."
  (let ((state (gethash (gptel-anthropic-oauth--get-backend-key backend)
                       gptel-anthropic-oauth--backend-state)))
    (plist-get state :tokens)))

(defun gptel-anthropic-oauth--set-mode (backend mode)
  "Store MODE for BACKEND."
  (let ((key (gptel-anthropic-oauth--get-backend-key backend))
        (state (gethash (gptel-anthropic-oauth--get-backend-key backend)
                       gptel-anthropic-oauth--backend-state
                       '())))
    (puthash key (plist-put state :mode mode)
            gptel-anthropic-oauth--backend-state)))

(defun gptel-anthropic-oauth--get-mode (backend)
  "Get mode for BACKEND."
  (let ((state (gethash (gptel-anthropic-oauth--get-backend-key backend)
                       gptel-anthropic-oauth--backend-state)))
    (plist-get state :mode)))

;;; PKCE Implementation

(defun gptel-anthropic-oauth--random-bytes (length)
  "Generate LENGTH random bytes."
  (let ((bytes (make-string length 0)))
    (dotimes (i length)
      (aset bytes i (random 256)))
    bytes))

(defun gptel-anthropic-oauth--base64url-encode (string)
  "Base64url encode STRING (RFC 4648).
This is base64 encoding with URL-safe characters and no padding."
  (let ((b64 (base64-encode-string string t)))
    (setq b64 (replace-regexp-in-string "+" "-" b64))
    (setq b64 (replace-regexp-in-string "/" "_" b64))
    (replace-regexp-in-string "=" "" b64)))

(defun gptel-anthropic-oauth--sha256 (string)
  "Compute SHA-256 hash of STRING."
  (secure-hash 'sha256 string nil nil t))

(defun gptel-anthropic-oauth--generate-pkce ()
  "Generate PKCE verifier and challenge.
Returns plist (:verifier VERIFIER :challenge CHALLENGE)."
  (let* ((verifier (gptel-anthropic-oauth--base64url-encode
                    (gptel-anthropic-oauth--random-bytes 63)))
         (challenge (gptel-anthropic-oauth--base64url-encode
                     (gptel-anthropic-oauth--sha256 verifier))))
    (list :verifier verifier :challenge challenge)))

;;; Token Storage

(defun gptel-anthropic-oauth--restore-tokens ()
  "Restore saved tokens from file."
  (when (file-exists-p gptel-anthropic-oauth-token-file)
    (let ((coding-system-for-read 'utf-8-auto-dos))
      (with-temp-buffer
        (insert-file-contents-literally gptel-anthropic-oauth-token-file)
        (goto-char (point-min))
        (read (current-buffer))))))

(defun gptel-anthropic-oauth--save-tokens (tokens)
  "Save TOKENS to file."
  (let ((print-length nil)
        (print-level nil)
        (coding-system-for-write 'utf-8-unix))
    (make-directory (file-name-directory gptel-anthropic-oauth-token-file) t)
    (with-temp-file gptel-anthropic-oauth-token-file
      (prin1 tokens (current-buffer)))
    tokens))

;;; OAuth Flow Functions

(defun gptel-anthropic-oauth--oauth-url (mode)
  "Generate OAuth authorization URL for MODE ('console or 'max).
Returns plist (:url URL :verifier VERIFIER)."
  (let* ((pkce (gptel-anthropic-oauth--generate-pkce))
         (verifier (plist-get pkce :verifier))
         (challenge (plist-get pkce :challenge))
         (base-url (if (eq mode 'console)
                       "https://console.anthropic.com"
                     "https://claude.ai"))
         (params `(("code" . "true")
                   ("client_id" . ,gptel-anthropic-oauth--client-id)
                   ("response_type" . "code")
                   ("redirect_uri" . ,gptel-anthropic-oauth--redirect-uri)
                   ("scope" . ,gptel-anthropic-oauth--scope)
                   ("code_challenge" . ,challenge)
                   ("code_challenge_method" . "S256")
                   ("state" . ,verifier)))
         (query-string (mapconcat (lambda (p)
                                   (concat (url-hexify-string (car p))
                                          "="
                                          (url-hexify-string (cdr p))))
                                 params "&")))
    (list :url (concat base-url "/oauth/authorize?" query-string)
          :verifier verifier)))

(defun gptel-anthropic-oauth--exchange-code (code verifier)
  "Exchange authorization CODE using VERIFIER.
Returns token plist."
  (let* ((code-parts (split-string code "#"))
         (auth-code (car code-parts))
         (state (cadr code-parts))
         (gptel-backend nil)  ; Unbind gptel-backend to avoid interference
         (response (gptel--url-retrieve
                       "https://console.anthropic.com/v1/oauth/token"
                     :method 'post
                     :data `(:grant_type "authorization_code"
                            :code ,auth-code
                            :state ,state
                            :client_id ,gptel-anthropic-oauth--client-id
                            :redirect_uri ,gptel-anthropic-oauth--redirect-uri
                            :code_verifier ,verifier))))
    (list :access-token (plist-get response :access_token)
          :refresh-token (plist-get response :refresh_token)
          :expires-at (+ (floor (float-time))
                        (plist-get response :expires_in)))))

(defun gptel-anthropic-oauth--refresh-token (refresh-token)
  "Refresh access token using REFRESH-TOKEN.
Returns updated token plist."
  (let ((gptel-backend nil)  ; Unbind gptel-backend to avoid interference
        (response (gptel--url-retrieve
                      "https://console.anthropic.com/v1/oauth/token"
                    :method 'post
                    :data `(:grant_type "refresh_token"
                           :refresh_token ,refresh-token
                           :client_id ,gptel-anthropic-oauth--client-id))))
    (list :access-token (plist-get response :access_token)
          :refresh-token (plist-get response :refresh_token)
          :expires-at (+ (floor (float-time))
                        (plist-get response :expires_in)))))

(defun gptel-anthropic-oauth--create-api-key (access-token)
  "Create static API key using ACCESS-TOKEN (console mode).
Returns API key string."
  (let ((gptel-backend nil)  ; Unbind gptel-backend to avoid interference
        (response (gptel--url-retrieve
                      "https://api.anthropic.com/api/oauth/claude_cli/create_api_key"
                    :method 'post
                    :headers `(("Authorization" . ,(concat "Bearer " access-token))
                              ("Content-Type" . "application/x-www-form-urlencoded")
                              ("Accept" . "application/json, text/plain, */*")))))
    (plist-get response :raw_key)))

;;; User Commands

;;;###autoload
(defun gptel-anthropic-login (&optional mode)
  "Login to Anthropic via OAuth.

Prompts for login MODE if not provided:
- 'console: Create static API key (Claude console)
- 'max: Use refresh tokens with auto-refresh (Claude Pro/Max)

This will open your browser for authorization, then prompt you
to paste the authorization code from the redirect URL."
  (interactive
   (list (intern (completing-read
                  "Anthropic login mode: "
                  '("console" "max")
                  nil t nil nil "max"))))

  ;; Find Anthropic backend
  (let ((backend (or (and (boundp 'gptel-backend)
                         gptel-backend
                         (gptel-anthropic-p gptel-backend)
                         gptel-backend)
                    (cl-find-if #'gptel-anthropic-p
                               (mapcar #'cdr gptel--known-backends))
                    (user-error "No Anthropic backend found.  \
Set one up with `gptel-make-anthropic' first"))))

    ;; Generate OAuth URL
    (pcase-let (((map :url :verifier)
                 (gptel-anthropic-oauth--oauth-url mode)))

      ;; Copy authorization URL to clipboard and open browser
      (gui-set-selection 'CLIPBOARD url)
      (read-from-minibuffer
       (format "Authorization URL copied. Press ENTER to open browser. \
If browser doesn't open, visit: %s " url))
      (browse-url url)

      ;; Wait for user to authorize and get code
      (let ((code (read-string "Paste authorization code from URL callback: ")))

        ;; Exchange code for tokens
        (message "Exchanging authorization code...")
        (condition-case err
            (let ((token-data (gptel-anthropic-oauth--exchange-code code verifier)))

              (cond
               ;; Console mode: create static API key
               ((eq mode 'console)
                (let ((api-key (gptel-anthropic-oauth--create-api-key
                               (plist-get token-data :access-token))))
                  (gptel-anthropic-oauth--set-tokens
                   backend
                   (gptel-anthropic-oauth--save-tokens
                    (list :mode 'console
                         :type 'auth/token
                         :api-key api-key
                         :created-at (floor (float-time)))))
                  (gptel-anthropic-oauth--set-mode backend 'console)
                  (message "Successfully logged in to Anthropic (console mode). \
Tokens stored in %s" gptel-anthropic-oauth-token-file)))

               ;; Max mode: store refresh token
               ((eq mode 'max)
                (gptel-anthropic-oauth--set-tokens
                 backend
                 (gptel-anthropic-oauth--save-tokens
                  (list :mode 'max
                       :type 'auth/oauth
                       :api-key (plist-get token-data :access-token)
                       :refresh-token (plist-get token-data :refresh-token)
                       :expires-at (plist-get token-data :expires-at)
                       :created-at (floor (float-time)))))
                (gptel-anthropic-oauth--set-mode backend 'max)
                (message "Successfully logged in to Anthropic (max mode). \
Tokens stored in %s" gptel-anthropic-oauth-token-file))))

          (error
           (user-error "Failed to exchange authorization code: %s"
                      (error-message-string err))))))))

;;;###autoload
(defun gptel-anthropic-logout ()
  "Clear Anthropic OAuth tokens and return to API key authentication."
  (interactive)
  (when (yes-or-no-p "Clear Anthropic OAuth tokens? ")
    (when (file-exists-p gptel-anthropic-oauth-token-file)
      (delete-file gptel-anthropic-oauth-token-file))
    (dolist (backend-entry gptel--known-backends)
      (let ((backend (cdr backend-entry)))
        (when (gptel-anthropic-p backend)
          (gptel-anthropic-oauth--set-tokens backend nil)
          (gptel-anthropic-oauth--set-mode backend nil))))
    (message "Cleared Anthropic OAuth tokens")))

;;; Header Function Advice

(defun gptel-anthropic-oauth--get-oauth-headers ()
  "Get OAuth headers if tokens are available."
  (when-let* ((backend gptel-backend)
              ((gptel-anthropic-p backend))
              (tokens (or (gptel-anthropic-oauth--get-tokens backend)
                         (gptel-anthropic-oauth--restore-tokens))))

    (let ((mode (plist-get tokens :mode))
          (auth-type (plist-get tokens :type))
          (api-key (plist-get tokens :api-key)))

      ;; Store tokens in backend if just loaded
      (unless (gptel-anthropic-oauth--get-tokens backend)
        (gptel-anthropic-oauth--set-tokens backend tokens)
        (gptel-anthropic-oauth--set-mode backend mode))

      ;; Auto-refresh if expired (max mode)
      (when (and (eq mode 'max)
                (eq auth-type 'auth/oauth)
                (plist-get tokens :expires-at)
                (> (floor (float-time))
                   (plist-get tokens :expires-at)))
        (condition-case err
            (let ((new-tokens (gptel-anthropic-oauth--refresh-token
                              (plist-get tokens :refresh-token))))
              (plist-put tokens :api-key (plist-get new-tokens :access-token))
              (plist-put tokens :refresh-token (plist-get new-tokens :refresh-token))
              (plist-put tokens :expires-at (plist-get new-tokens :expires-at))
              (gptel-anthropic-oauth--save-tokens tokens)
              (setq api-key (plist-get new-tokens :access-token)))
          (error
           (gptel-anthropic-oauth--set-tokens backend nil)
           (gptel-anthropic-oauth--save-tokens nil)
           (message "Token refresh failed: %s. Please run M-x gptel-anthropic-login"
                   (error-message-string err))
           (signal (car err) (cdr err)))))

      ;; Return appropriate headers (matching ECA)
      (if (eq auth-type 'auth/oauth)
          ;; Max mode: Bearer token with oauth beta header
          `(("Authorization" . ,(concat "Bearer " api-key))
            ("anthropic-version" . "2023-06-01")
            ("anthropic-beta" . "oauth-2025-04-20"))
        ;; Console mode: x-api-key only (no beta header)
        `(("x-api-key" . ,api-key)
          ("anthropic-version" . "2023-06-01"))))))

(defconst gptel-anthropic-oauth--system-prefix
  "You are Claude Code, Anthropic's official CLI for Claude."
  "System message prefix required for OAuth tokens to work.")

(defun gptel-anthropic-oauth--using-oauth-p ()
  "Return non-nil if current request uses OAuth authentication.
Also restores tokens from disk if not already loaded."
  (and (boundp 'gptel-backend)
       gptel-backend
       (gptel-anthropic-p gptel-backend)
       (let ((tokens (or (gptel-anthropic-oauth--get-tokens gptel-backend)
                         ;; Try to restore from disk
                         (when-let ((restored (gptel-anthropic-oauth--restore-tokens)))
                           (gptel-anthropic-oauth--set-tokens gptel-backend restored)
                           (gptel-anthropic-oauth--set-mode gptel-backend
                                                            (plist-get restored :mode))
                           restored))))
         (and tokens (eq (plist-get tokens :type) 'auth/oauth)))))

(defun gptel-anthropic-oauth--request-data-advice (orig-fn backend prompts)
  "Advice to prepend Claude Code system message for OAuth requests.
ORIG-FN is the original `gptel--request-data' method.
BACKEND and PROMPTS are passed through."
  (if (gptel-anthropic-oauth--using-oauth-p)
      ;; Prepend Claude Code identification to system message
      (let ((gptel--system-message
             (if gptel--system-message
                 (if (consp gptel--system-message)
                     (cons gptel-anthropic-oauth--system-prefix
                           gptel--system-message)
                   (list gptel-anthropic-oauth--system-prefix
                         gptel--system-message))
               gptel-anthropic-oauth--system-prefix)))
        (funcall orig-fn backend prompts))
    (funcall orig-fn backend prompts)))

(defun gptel-anthropic-oauth--curl-advice (orig-fn fsm)
  "Advice to inject OAuth headers for Anthropic backends.
ORIG-FN is the original `gptel-curl-get-response' function.
FSM is the state machine driving this request."
  (if (gptel-anthropic-oauth--using-oauth-p)
      ;; Replace the header function temporarily with OAuth headers
      (let ((orig-header-fn (gptel-backend-header gptel-backend)))
        (setf (gptel-backend-header gptel-backend)
              (lambda () (gptel-anthropic-oauth--get-oauth-headers)))
        (unwind-protect
            (funcall orig-fn fsm)
          (setf (gptel-backend-header gptel-backend) orig-header-fn)))
    (funcall orig-fn fsm)))

(defun gptel-anthropic-oauth--enable ()
  "Enable OAuth support for Anthropic backends."
  ;; Add advice to inject OAuth headers
  (advice-add 'gptel-curl-get-response :around
              #'gptel-anthropic-oauth--curl-advice)
  ;; Add advice to inject system message for OAuth
  (advice-add 'gptel--request-data :around
              #'gptel-anthropic-oauth--request-data-advice))

;;; Auto-enable

;; Auto-enable when loaded
(with-eval-after-load 'gptel
  (gptel-anthropic-oauth--enable))

(provide 'gptel-anthropic-oauth)
;;; gptel-anthropic-oauth.el ends here
