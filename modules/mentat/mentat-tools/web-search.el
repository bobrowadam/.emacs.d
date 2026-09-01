;;; web-search.el --- Asynchronous Exa and Jina web search -*- lexical-binding: t; -*-

(require 'auth-source)
(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'url)
(require 'url-http)
(require 'mentat-elisp-library)
(require 'tool-result)

(defconst mentat-web-search-exa-endpoint "https://api.exa.ai/search"
  "Exa Search API endpoint.")

(defconst mentat-web-search-jina-endpoint "https://r.jina.ai/"
  "Jina Reader API endpoint.")

(defvar mentat-web-search-exa-auth-hosts '("api.exa.ai")
  "Auth-source hosts checked for an Exa API key.")

(defun mentat-web-search--api-key (provider hosts)
  "Return PROVIDER's API key from the first matching auth-source HOSTS entry."
  (or (cl-loop
       for host in hosts
       for entry = (car (auth-source-search :host host :max 1
                                             :require '(:secret)))
       when entry return (auth-info-password entry))
      (user-error "No %s credential found in Emacs auth-source (checked %s)"
                  provider (string-join hosts ", "))))

(defun mentat-web-search--response-body ()
  "Move past HTTP headers and return the current response body."
  (goto-char (point-min))
  (unless (re-search-forward "\r?\n\r?\n" nil t)
    (error "HTTP response has no header terminator"))
  (mentat-tool-result-decode-utf8
   (buffer-substring-no-properties (point) (point-max))))

(defun mentat-web-search--handle-response (success error parser status)
  "Handle an HTTP response with SUCCESS, ERROR, PARSER, and STATUS."
  (unwind-protect
      (condition-case err
          (if-let* ((request-error (plist-get status :error)))
              (funcall error (format "Request failed: %S" request-error))
            (let* ((http-status (or url-http-response-status 0))
                   (body (mentat-web-search--response-body)))
              (if (and (>= http-status 200) (< http-status 300))
                  (funcall success (funcall parser body))
                (funcall error
                         (format "HTTP %d: %s"
                                 http-status
                                 (truncate-string-to-width body 1000 nil nil t))))))
        (error (funcall error (error-message-string err))))
    (kill-buffer (current-buffer))))

(defun mentat-web-search--request (url method headers data parser success error)
  "Request URL using METHOD, HEADERS, and DATA, then invoke SUCCESS or ERROR.
PARSER converts the successful response body into the public result."
  (let ((url-request-method method)
        (url-request-extra-headers headers)
        (url-request-data data))
    (url-retrieve url
                  (apply-partially #'mentat-web-search--handle-response
                                   success error parser)
                  nil t t)))

(defun mentat-web-search--cancel-request (buffer)
  "Cancel the web request associated with BUFFER."
  (when (buffer-live-p buffer)
    (kill-buffer buffer)))

(defun mentat-web-search--starter (start)
  "Return an asynchronous Mentat starter around callback function START."
  (lambda (resolve reject on-cancel)
    (let ((request (funcall start resolve reject)))
      (funcall on-cancel
               (lambda () (mentat-web-search--cancel-request request))))))

(defun mentat-web-search--exa-result (result)
  "Normalize one Exa RESULT for tool output."
  (let ((highlights (alist-get 'highlights result)))
    `((title . ,(or (alist-get 'title result) ""))
      (url . ,(or (alist-get 'url result) ""))
      (published-date . ,(alist-get 'publishedDate result))
      (author . ,(alist-get 'author result))
      (summary . ,(alist-get 'summary result))
      (highlights . ,(if (listp highlights) highlights nil)))))

(defun mentat-web-search--parse-exa (query body)
  "Parse Exa response BODY for QUERY."
  (let* ((payload (json-parse-string body :object-type 'alist
                                     :array-type 'list
                                     :null-object nil
                                     :false-object nil))
         (results (alist-get 'results payload)))
    `((provider . "exa")
      (query . ,query)
      (results . ,(mapcar #'mentat-web-search--exa-result results))
      (request-id . ,(alist-get 'requestId payload))
      (cost-dollars . ,(alist-get 'costDollars payload)))))

(defun mentat-web-search--jina-field (field body)
  "Return FIELD from Jina Reader response BODY, or nil."
  (when (string-match (format "^%s:[ \t]*\\(.+\\)$" (regexp-quote field)) body)
    (string-trim (match-string 1 body))))

(defun mentat-web-search--parse-jina (requested-url body)
  "Parse Jina Reader response BODY for REQUESTED-URL."
  (let* ((marker "Markdown Content:")
         (marker-position (string-match (concat "^" marker "[ \t]*$") body))
         (content (if marker-position
                      (string-trim-left
                       (substring body (match-end 0)))
                    body)))
    `((provider . "jina-reader")
      (requested-url . ,requested-url)
      (title . ,(mentat-web-search--jina-field "Title" body))
      (source-url . ,(mentat-web-search--jina-field "URL Source" body))
      ,@(mentat-tool-result-limit-text content :prefix "jina-reader"))))

(defun mentat-web-search--exa-start
    (query num-results type include-domains start-published-date
           end-published-date max-age-hours success error)
  "Start an Exa search and invoke SUCCESS or ERROR when it completes."
  (unless (and (stringp query) (not (string-empty-p (string-trim query))))
    (user-error "Exa search query must not be empty"))
  (unless (and (integerp num-results) (<= 1 num-results 20))
    (user-error "Exa num-results must be an integer from 1 through 20"))
  (unless (member type '("instant" "fast" "auto" "deep-lite" "deep"
                         "deep-reasoning"))
    (user-error "Unknown Exa search type: %s" type))
  (when (and max-age-hours
             (not (and (integerp max-age-hours)
                       (<= -1 max-age-hours 720))))
    (user-error "Exa max-age-hours must be an integer from -1 through 720"))
  (let* ((api-key (mentat-web-search--api-key
                   "Exa" mentat-web-search-exa-auth-hosts))
         (contents `((highlights . ((maxCharacters . 2000)))
                     ,@(when max-age-hours
                         `((maxAgeHours . ,max-age-hours)))))
         (payload `((query . ,query)
                    (numResults . ,num-results)
                    (type . ,type)
                    (contents . ,contents)
                    ,@(when include-domains
                        `((includeDomains . ,(vconcat include-domains))))
                    ,@(when start-published-date
                        `((startPublishedDate . ,start-published-date)))
                    ,@(when end-published-date
                        `((endPublishedDate . ,end-published-date))))))
    (mentat-web-search--request
     mentat-web-search-exa-endpoint "POST"
     `(("Content-Type" . "application/json")
       ("x-api-key" . ,api-key))
     (encode-coding-string (json-serialize payload) 'utf-8)
     (apply-partially #'mentat-web-search--parse-exa query)
     success error)))

(defun mentat-web-search--jina-start (url success error)
  "Start a Jina Reader request for URL and invoke SUCCESS or ERROR."
  (unless (and (stringp url) (not (string-empty-p (string-trim url))))
    (user-error "Jina Reader URL must not be empty"))
  (let* ((trimmed-url (string-trim url))
         (source-url (if (string-match-p "\\`https?://" trimmed-url)
                         trimmed-url
                       (concat "https://" trimmed-url)))
         (reader-url (concat mentat-web-search-jina-endpoint source-url)))
    (mentat-web-search--request
     reader-url "GET"
     '(("Accept" . "text/plain; charset=utf-8")
       ("User-Agent" . "mentat-web-search/1.0"))
     nil
     (apply-partially #'mentat-web-search--parse-jina source-url)
     success error)))

(mentat-defun mentat-exa-search
    (query &key (num-results 5) (type "auto") include-domains
           start-published-date end-published-date max-age-hours)
  "Search the web with Exa and return ranked sources and highlights.
QUERY is the search query.  NUM-RESULTS is from 1 to 20.  TYPE is one of
instant, fast, auto, deep-lite, deep, or deep-reasoning.  INCLUDE-DOMAINS
restricts results to a list of domains.  START-PUBLISHED-DATE and
END-PUBLISHED-DATE are ISO 8601 timestamps.  MAX-AGE-HOURS controls Exa's
content cache; zero requests fresh content and -1 always uses cached content."
  (:execution async :display "Exa web search")
  (mentat-web-search--starter
   (lambda (resolve reject)
     (mentat-web-search--exa-start
      query num-results type include-domains start-published-date
      end-published-date max-age-hours resolve reject))))

(mentat-defun mentat-jina-read (url)
  "Fetch URL through Jina Reader and return clean LLM-ready Markdown.
URLs without a scheme use HTTPS.  Returned content is bounded to 50,000
characters and reports whether it was truncated."
  (:execution async :display "Read with Jina")
  (mentat-web-search--starter
   (lambda (resolve reject)
     (mentat-web-search--jina-start url resolve reject))))

(provide 'web-search)
;;; web-search.el ends here
