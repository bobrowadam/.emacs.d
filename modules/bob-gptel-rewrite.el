;;; bob-gptel-rewrite.el --- Fast inline rewrite/insert for gptel -*- lexical-binding: t; -*-

;; Inline rewrite and insert commands for gptel, styled after Cursor's
;; Cmd-K and CodeCompanion.nvim's inline strategy: small, fast,
;; non-thinking model for bounded edits; reserve the big thinking
;; model for chat/agent work.
;;
;; This module provides:
;;
;; - A dedicated "Claude rewrite" Anthropic backend: streaming, no
;;   adaptive thinking, no server-side web/code tools.  See
;;   `bob/gptel-rewrite-backend'.
;;
;; - A `rewrite' gptel preset bound to Haiku 4.5, with read-only
;;   context-discovery tools (`read', `ls', `find', `grep') and the
;;   current buffer auto-attached so the model sees the whole file.
;;   See `bob/gptel-rewrite-tools'.
;;
;; - Advice on `gptel--suffix-rewrite' that applies the preset around
;;   every `gptel-rewrite' request -- you do not have to select the
;;   preset manually.
;;
;; - `bob/gptel-insert': the same overlay / dispatch-transient UX as
;;   `gptel-rewrite' but for inserting NEW text at point instead of
;;   replacing a region.  Sends the full buffer with a <POINT/>
;;   marker and uses an insert-specific system directive.  Prefix arg
;;   upgrades from Haiku to Sonnet 4.6.
;;
;; - `bob/gptel-sanitize-llm-output': registered on
;;   `gptel-post-rewrite-functions' to strip the most common "I told
;;   you NOT to" artifacts -- markdown fences that wrap the whole
;;   response, plus leading/trailing blank lines.  Helps both
;;   `gptel-rewrite' and `bob/gptel-insert'.
;;
;; Usage (after `gptel' is installed and configured):
;;
;;   (require 'bob-gptel-rewrite)
;;   (bob/gptel-rewrite-install)
;;
;; Then bind `bob/gptel-insert' to a convenient key (e.g. C-c g i).
;; `gptel-rewrite' already exists upstream; the advice activates
;; transparently.

;;; Code:

(require 'gptel)
(require 'cl-lib)
(require 'subr-x)

(defgroup bob-gptel-rewrite nil
  "Fast inline rewrite/insert commands for gptel."
  :group 'gptel)

;;; Backend ------------------------------------------------------------

(defvar bob/gptel-rewrite-backend
  (let ((b (gptel-make-anthropic "Claude rewrite"
             :stream t
             ;; No adaptive thinking (rewrites are bounded,
             ;; latency-sensitive) and no server-side web/code tools
             ;; (out of scope for inline edits; the preset attaches
             ;; local read-only tools instead).
             :request-params '(:max_tokens 8192))))
    ;; The `gptel-anthropic-oauth' module injects OAuth headers via
    ;; :around advice on `gptel-curl-get-response'.  That advice
    ;; swaps the header function on the dynamic `gptel-backend'
    ;; variable -- but by the time the advice runs on a follow-up
    ;; (after a tool call), `gptel-backend' is the global default
    ;; ("Claude thinking"), NOT this rewrite backend.  Inside
    ;; `gptel-curl-get-response' the backend is then rebound to the
    ;; FSM's backend (us), whose header is never swapped, so the
    ;; request goes out without x-api-key and 401s.
    ;;
    ;; Pin a header function directly on this backend that always
    ;; uses `gptel-anthropic-oauth--get-oauth-headers'.  That makes
    ;; auth work on both the first request and every tool-result
    ;; follow-up, regardless of what `gptel-backend' points at.
    (when (fboundp 'gptel-anthropic-oauth--get-oauth-headers)
      (setf (gptel-backend-header b)
            (lambda (_info)
              (let ((gptel-backend b))
                (gptel-anthropic-oauth--get-oauth-headers)))))
    b)
  "Non-thinking Anthropic backend used by the `rewrite' gptel preset.")

;;; Tools --------------------------------------------------------------

(defcustom bob/gptel-rewrite-tools '("read" "ls" "find" "grep")
  "Gptel tool names available during `gptel-rewrite' / `bob/gptel-insert'.

Only read-only context-discovery tools by default.  Omitted on
purpose:

- `write' / `edit' -- rewrite already owns the edit.
- `bash' / `elisp_eval' -- side-effectful.
- `jina_reader' -- web, not local context.

Tools must be registered (via `gptel-make-tool') before the
`rewrite' preset is applied."
  :type '(repeat string)
  :group 'bob-gptel-rewrite)

;;; Preset advice ------------------------------------------------------

(defun bob/gptel-rewrite-with-preset (orig &rest args)
  "Apply the `rewrite' gptel preset for the duration of ORIG.

Used as :around advice on `gptel--suffix-rewrite' so every
`gptel-rewrite' request picks up Haiku, read-only tools and the
current buffer as context.

As a special case: when the overlay at point was created by
`bob/gptel-insert', upstream's suffix would feed the current
response back as the thing to rewrite -- wrong semantics for
insert.  In that case we reject the existing insert overlay and
fire a fresh `bob/gptel-insert' at the same point with the new
instruction."
  (let ((insert-ov (cdr-safe (get-char-property-and-overlay
                              (point) 'bob/gptel-insert-placeholder))))
    (if insert-ov
        (let ((beg (overlay-get insert-ov 'bob/gptel-insert-beg))
              (instruction (or (car args) gptel--rewrite-message)))
          (goto-char beg)
          (gptel--rewrite-reject insert-ov)
          (bob/gptel-insert instruction))
      ;; Work around upstream oversight: `gptel--rewrite-handlers'
      ;; defines a `TPRE' handler for `gptel-pre-tool-call-functions',
      ;; but the default `gptel-request--transitions' table never
      ;; enters `TPRE'.  Rebind to `gptel-send--transitions' (which
      ;; includes TPRE) while constructing the rewrite FSM so the
      ;; pre-tool-call hook fires as intended.
      (let ((gptel-request--transitions
             (if (boundp 'gptel-send--transitions)
                 gptel-send--transitions
               gptel-request--transitions)))
        (gptel-with-preset 'rewrite (apply orig args))))))

(defun bob/gptel-rewrite-dispatch-fix-iterate (orig &optional ov ci)
  "Intercept the 'iterate' branch of `gptel--rewrite-dispatch'.

Upstream has a latent bug: when `gptel-rewrite-default-action' is
`dispatch', the response handler calls
`(gptel--rewrite-dispatch ov)' with CI nil.  If the user then
picks `iterate', dispatch calls `(gptel--rewrite-iterate ov)',
which is an alias for the `gptel-rewrite' transient prefix -- a
zero-argument command.  That raises
  (wrong-number-of-arguments #<subr gptel-rewrite> 1).

We fix this by reading the user's action ourselves; if they pick
`iterate', we handle it:

- For a `bob/gptel-insert' overlay, reject it and fire a fresh
  `bob/gptel-insert' with a newly-prompted instruction (preserves
  insert semantics instead of feeding the response back).

- For a regular rewrite overlay, invoke the `gptel-rewrite'
  transient interactively -- the only way a transient prefix can
  be launched.

Any other choice (accept / reject / diff / ediff / merge) is
dispatched by calling the corresponding `gptel--rewrite-*' helper
directly with OV, mirroring upstream's own non-interactive path."
  (if (or ci (null ov))
      ;; Interactive path or no overlay -- upstream's own handling is
      ;; fine; `iterate' goes through `call-interactively'.
      (funcall orig ov ci)
    ;; Non-interactive path: read the choice ourselves.
    (let* ((orig-status (copy-sequence (overlay-get ov 'status)))
           (choices '((?a "accept") (?k "reject") (?r "iterate")
                      (?m "merge") (?d "diff") (?e "ediff")))
           (choice
            (unwind-protect
                (progn
                  (gptel--rewrite-update-status
                   ov (when (fboundp 'rmc--add-key-description)
                        (concat " "
                                (mapconcat
                                 #'cdr
                                 (mapcar #'rmc--add-key-description choices)
                                 ", "))))
                  (read-multiple-choice "Action: " choices))
              (overlay-put ov 'status orig-status)
              (overlay-put ov 'before-string
                           (apply #'concat orig-status))))
           (action (cadr choice)))
      (cond
       ;; Iterate on an insert overlay: fresh insert at same point.
       ((and (string= action "iterate")
             (overlay-get ov 'bob/gptel-insert-placeholder))
        (let ((beg (overlay-get ov 'bob/gptel-insert-beg))
              (instruction (read-string "Iterate insert: ")))
          (goto-char beg)
          (gptel--rewrite-reject ov)
          (bob/gptel-insert instruction)))
       ;; Iterate on a regular rewrite overlay: open the transient.
       ((string= action "iterate")
        (goto-char (overlay-start ov))
        (call-interactively #'gptel-rewrite))
       ;; Everything else: call the corresponding helper with OV.
       (t (funcall (intern (concat "gptel--rewrite-" action)) ov))))))

;;; Language detection ----------------------------------------------

;; Upstream's `gptel--strip-mode-suffix' only returns a language name
;; if the major mode derives from `prog-mode'/`text-mode'/`tex-mode'.
;; Several Emacs 29/30 treesit modes (`rust-ts-mode', `go-ts-mode',
;; `json-ts-mode', `html-ts-mode', `css-ts-mode', ...) do NOT set
;; `derived-mode-parent' and fail that check, so the directive ends
;; up with an empty language string.  We work around this plus handle
;; a few other useful cases (org src blocks, file extension fallback).

(defconst bob/gptel-rewrite-extension-lang-alist
  '(("ts"   . "typescript")
    ("tsx"  . "tsx")
    ("js"   . "javascript")
    ("jsx"  . "jsx")
    ("mjs"  . "javascript")
    ("py"   . "python")
    ("rs"   . "rust")
    ("go"   . "go")
    ("rb"   . "ruby")
    ("java" . "java")
    ("kt"   . "kotlin")
    ("swift". "swift")
    ("c"    . "c")
    ("h"    . "c")
    ("cpp"  . "c++")
    ("cc"   . "c++")
    ("hpp"  . "c++")
    ("cs"   . "c#")
    ("php"  . "php")
    ("sh"   . "bash")
    ("bash" . "bash")
    ("zsh"  . "zsh")
    ("fish" . "fish")
    ("lua"  . "lua")
    ("el"   . "emacs-lisp")
    ("lisp" . "common-lisp")
    ("clj"  . "clojure")
    ("cljs" . "clojurescript")
    ("hs"   . "haskell")
    ("ml"   . "ocaml")
    ("scala". "scala")
    ("dart" . "dart")
    ("ex"   . "elixir")
    ("exs"  . "elixir")
    ("erl"  . "erlang")
    ("json" . "json")
    ("yaml" . "yaml")
    ("yml"  . "yaml")
    ("toml" . "toml")
    ("html" . "html")
    ("css"  . "css")
    ("scss" . "scss")
    ("md"   . "markdown")
    ("sql"  . "sql"))
  "File-extension to language-name fallback for `bob/gptel-rewrite-mode-language'.")

(defun bob/gptel-rewrite-mode-language ()
  "Return a best-effort language label for the current buffer/point.

Checks, in order:

1. Org-mode source blocks: if point is inside `#+begin_src TYPE',
   return TYPE.
2. `gptel--strip-mode-suffix' for well-behaved modes.
3. `*-ts-mode' treesitter modes where the upstream derivation
   gate fails: strip `-ts-mode' / `-mode' anyway.
4. `buffer-file-name' extension via
   `bob/gptel-rewrite-extension-lang-alist'.
5. Empty string.

Always returns a (possibly empty) string."
  (let ((lang
         (or
          ;; 1. Org src block.
          (when (and (derived-mode-p 'org-mode) (fboundp 'org-element-at-point))
            (let ((el (ignore-errors (org-element-at-point))))
              (when (and el (eq (org-element-type el) 'src-block))
                (org-element-property :language el))))
          ;; 2. Upstream helper (handles the override alist + prog/text
          ;;    derivation).
          (let ((s (gptel--strip-mode-suffix major-mode)))
            (and (stringp s) (not (string-empty-p s)) s))
          ;; 3. Treesit mode fallback: strip -ts-mode or -mode.
          (let ((name (symbol-name major-mode)))
            (cond
             ((string-suffix-p "-ts-mode" name)
              (string-remove-suffix "-ts-mode" name))
             ((string-suffix-p "-mode" name)
              (let ((stripped (string-remove-suffix "-mode" name)))
                ;; Don't accept random special modes like `dired' or
                ;; `compilation' here.  Only use the extension
                ;; fallback below for those.
                (when (or (derived-mode-p 'prog-mode)
                          (derived-mode-p 'text-mode)
                          (derived-mode-p 'tex-mode))
                  stripped)))))
          ;; 4. File extension.
          (when-let* ((f (buffer-file-name))
                      (ext (file-name-extension f)))
            (alist-get (downcase ext)
                       bob/gptel-rewrite-extension-lang-alist
                       nil nil #'string=)))))
    (downcase (or lang ""))))

(defun bob/gptel-rewrite-mode-is-programmingish ()
  "Non-nil if the current buffer/point looks like code.

Used to pick the programmer-vs-editor branch of the insert
directive.  Treats org src blocks and treesit modes as
programming even when upstream's `prog-mode' derivation check
fails."
  (or (derived-mode-p 'prog-mode)
      (string-suffix-p "-ts-mode" (symbol-name major-mode))
      (and (derived-mode-p 'org-mode)
           (fboundp 'org-element-at-point)
           (let ((el (ignore-errors (org-element-at-point))))
             (and el (eq (org-element-type el) 'src-block))))
      ;; File extension known to be a programming language -- catches
      ;; buffers in `fundamental-mode' over a .rs file, etc.
      (when-let* ((f (buffer-file-name))
                  (ext (file-name-extension f)))
        (assoc (downcase ext) bob/gptel-rewrite-extension-lang-alist))))

;;; Insert: directive --------------------------------------------------

(defun bob/gptel-insert-directive ()
  "Return the system directive for `bob/gptel-insert'.

Specialised for programming buffers (including treesit modes and
org src blocks): reinforces the \"output only the text to insert,
no prose, no fences\" contract with an example.  Other modes get
a shorter prose-focused variant."
  (let* ((lang (bob/gptel-rewrite-mode-language))
         (article (if (and lang (not (string-empty-p lang))
                           (memq (aref lang 0) '(?a ?e ?i ?o ?u)))
                      "an" "a")))
    (if (bob/gptel-rewrite-mode-is-programmingish)
        (format (concat
                 "You are %s %s programmer.  Your ONLY job is to output text that will be inserted verbatim into a file at a specific location.\n"
                 "\n"
                 "INPUT FORMAT: You receive the full current file contents with the insertion point marked as the literal token <POINT/>, followed by an instruction describing what to insert there.\n"
                 "\n"
                 "OUTPUT CONTRACT (strict):\n"
                 "- Output ONLY the %s text that should replace the <POINT/> marker.  Nothing else.\n"
                 "- Do NOT repeat, quote, or echo any surrounding code from the file.\n"
                 "- Do NOT wrap the output in markdown fences (no ```).\n"
                 "- Do NOT add commentary, prose, explanations, apologies, progress reports, or meta-discussion.  Not before, not after, not as comments.\n"
                 "- Do NOT think out loud or reconsider in the output.  Decide silently, then write only the final text.\n"
                 "- Match the surrounding indentation, naming style, and conventions.\n"
                 "- If the instruction is ambiguous or the insertion point seems odd, make the most reasonable assumption and produce the insertion anyway.  Never ask a clarifying question in the output.\n"
                 "- If the instruction would require rewriting existing surrounding code (not just inserting new text), output only the minimal NEW text that achieves the goal at <POINT/>; the user can follow up with a separate rewrite.\n"
                 "\n"
                 "EXAMPLE\n"
                 "Input file:\n"
                 "  (defun square (x)\n"
                 "    <POINT/>\n"
                 "    (* x x))\n"
                 "Instruction: add a docstring\n"
                 "\n"
                 "WRONG output (do NOT do any of this):\n"
                 "  Now I'll add a docstring:\n"
                 "\n"
                 "  ```lisp\n"
                 "  \"Return the square of X.\"\n"
                 "  ```\n"
                 "\n"
                 "  Since you asked for only the text, here it is:\n"
                 "  \"Return the square of X.\"\n"
                 "\n"
                 "CORRECT output (the ENTIRE response, first character to last):\n"
                 "  \"Return the square of X.\"\n"
                 "\n"
                 "Your very first character must be the first character of the code to insert.  Your very last character must be the last character of the code to insert.  No introductions.  No restatements.  No \"however\".  No \"since you asked\".  Emit the code once, cleanly, and stop.")
                article lang lang)
      (concat
       (if (string-empty-p lang)
           "You are an editor."
         (format "You are %s %s editor." article lang))
       "  Your ONLY job is to output text that will be inserted verbatim into a document at the <POINT/> marker."
       "  Output ONLY the replacement text, no markdown fences, no commentary, no explanations."
       "  Do not repeat surrounding content.  Match the surrounding style."
       "  If the instruction is ambiguous, make a reasonable assumption and produce the insertion anyway."))))

;;; Sanitiser ----------------------------------------------------------

(defun bob/gptel-sanitize--all-fence-bodies ()
  "Return a list of (BEG . END) for every fenced code block.

Searches the current narrowed buffer for \"```LANG?\\n...\\n```\"
blocks; BEG and END delimit the body *inside* the fences, not the
fence lines themselves.  Unterminated opening fences are ignored."
  (save-excursion
    (goto-char (point-min))
    (let (bodies)
      (while (re-search-forward "^[ \t]*```[^\n]*\n" nil t)
        (let ((body-start (match-end 0)))
          (when (re-search-forward "^[ \t]*```[ \t]*$" nil t)
            (push (cons body-start (match-beginning 0)) bodies))))
      (nreverse bodies))))

(defun bob/gptel-sanitize-llm-output (beg end)
  "Sanitize LLM output in region BEG..END (rewrite/insert temp buffer).

Runs from `gptel-post-rewrite-functions', inside the rewrite temp
buffer, before the overlay is populated -- so both the streamed
preview and the eventually-accepted text are cleaned.

Small models (Haiku in particular) ignore \"no prose, no fences\"
directives and emit patterns like:

    Sure!  Here you go:

    ```lisp
    (defun foo () 42)
    ```

    However, since you asked for only the text...

    (defun foo () 42)

This sanitizer handles the common shapes:

1. If any fenced code blocks are present, keep the body of the
   LAST one.  Haiku typically puts a cleaner restatement last;
   more importantly, the contents of a fenced block are
   guaranteed to be code and nothing else.
2. If NO fenced blocks are present but the output starts with a
   paragraph of prose followed by a blank line, drop everything
   up to and including the first blank line.  Heuristic: if the
   first non-blank line doesn't look like code (no parens,
   braces, indentation, or common code starters) and a later
   line does, drop the leading prose.
3. Trim surrounding blank lines."
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      ;; 1. Prefer fenced code blocks when present.
      (let ((bodies (bob/gptel-sanitize--all-fence-bodies)))
        (when bodies
          (let* ((last (car (last bodies)))
                 (body (buffer-substring-no-properties (car last) (cdr last))))
            (delete-region (point-min) (point-max))
            (insert body))))
      ;; 2. No fences?  Try to drop leading prose.
      (unless (save-excursion
                (goto-char (point-min))
                (re-search-forward "^[ \t]*```" nil t))
        (goto-char (point-min))
        (skip-chars-forward " \t\n")
        (let* ((first-line-start (point))
               (first-line-end (line-end-position))
               (first-line (buffer-substring-no-properties
                            first-line-start first-line-end))
               ;; "Looks like prose": starts with a capital letter or
               ;; digit followed by a space and no opening paren/brace
               ;; on the line, AND doesn't start with common code
               ;; markers.
               (code-starter-re
                (rx bos (zero-or-more (any " \t"))
                    (or "(" ")" "[" "{" "#" ";" "/" "*"
                        "def " "class " "fn " "let " "const " "var "
                        "function " "import " "use " "package "
                        "public " "private " "protected " "static "
                        "if " "for " "while " "return "
                        "@" "-" "+" "--" "//")))
               (prose-p (and (not (string-match-p code-starter-re first-line))
                             (string-match-p "[[:alpha:]]" first-line))))
          (when prose-p
            ;; Find the first subsequent line that looks like code and
            ;; is preceded by a blank line.
            (when (re-search-forward
                   (rx "\n\n"
                       (group (zero-or-more (any " \t"))
                              (or "(" ";" "#" "/" "\""
                                  "def " "class " "fn " "let "
                                  "const " "var " "function "
                                  "import " "use " "package "
                                  "public " "private ")))
                   nil t)
              (delete-region (point-min) (match-beginning 1))))))
      ;; 3. Trim surrounding blank lines.
      (goto-char (point-min))
      (when (looking-at "[ \t]*\n+")
        (replace-match ""))
      (goto-char (point-max))
      (when (re-search-backward "\n[ \t\n]+\\'" nil t)
        (replace-match "\n")))))

;;; Insert command -----------------------------------------------------

;; A zero-width overlay is invisible to `get-char-property', which
;; means upstream's dispatch/iterate/diff helpers (they all do
;; `(get-char-property (point) 'gptel-rewrite)' to find the overlay)
;; don't see our insert overlays.  Workaround: insert a single space
;; placeholder at point and make the overlay span it.  Accept then
;; replaces the placeholder with the response via upstream's
;; `(delete-region ov-beg ov-end) (insert response)'; reject needs
;; advice to delete the placeholder too (see
;; `bob/gptel-rewrite-reject-cleans-insert-placeholder').
(defconst bob/gptel-insert-placeholder " "
  "Single-character placeholder inserted at point for `bob/gptel-insert'.
Must be exactly one character long so accept/reject overlay math
stays trivial.")

(defun bob/gptel-rewrite-reject-cleans-insert-placeholder (&optional ovs)
  "Delete the `bob/gptel-insert' placeholder after rejecting the overlay.

Advice on `gptel--rewrite-reject'.  Upstream's reject only deletes
the overlay; for our insert overlays the placeholder character
needs to go too, otherwise the buffer keeps accumulating stray
spaces."
  (dolist (ov (ensure-list ovs))
    (when (overlay-get ov 'bob/gptel-insert-placeholder)
      (let ((beg (overlay-get ov 'bob/gptel-insert-beg))
            (end (overlay-get ov 'bob/gptel-insert-end))
            (buf (overlay-buffer ov)))
        (when (and beg end buf (buffer-live-p buf))
          (with-current-buffer buf
            (save-excursion
              (delete-region beg end))))))))

(defun bob/gptel-rewrite-accept-cleans-overlay (&optional ovs &rest _)
  "Delete the overlay after accepting a rewrite/insert response.

Advice on `gptel--rewrite-accept'.  Upstream leaves the overlay
in place after accept, which keeps a stale highlight face and
keymap on the accepted text -- surprising for our insert flow.
Running after accept, we scrub any overlay whose response has
already been committed."
  (dolist (ov (ensure-list ovs))
    (when (overlayp ov)
      (setq gptel--rewrite-overlays (delq ov gptel--rewrite-overlays))
      (delete-overlay ov)))
  (unless gptel--rewrite-overlays
    (remove-hook 'eldoc-documentation-functions
                 #'gptel--rewrite-key-help 'local)))

;;;###autoload
(defun bob/gptel-insert (instruction &optional strong)
  "Insert LLM-generated text at point, driven by INSTRUCTION.

Shows the same accept/reject/diff/iterate overlay UX as
`gptel-rewrite', using the `rewrite' preset (Haiku + read-only
tools).  The full buffer is sent with a <POINT/> marker so the
model knows exactly where the insertion goes.

With a prefix argument STRONG, use Claude Sonnet 4.6 instead of
Haiku 4.5 -- useful for non-trivial inserts where Haiku stumbles."
  (interactive (list (read-string (if current-prefix-arg
                                      "Insert (Sonnet): "
                                    "Insert: "))
                     current-prefix-arg))
  (when (string-empty-p (string-trim instruction))
    (user-error "Empty instruction"))
  ;; We reuse the rewrite FSM handlers and callback.
  (require 'gptel-rewrite)
  ;; Outer preset provides backend/tools/use-tools; the inline spec
  ;; with `:parents' composes on top and lets us optionally upgrade
  ;; the model to Sonnet for this one request.
  (gptel-with-preset
      (if strong
          '(:model claude-sonnet-4-6 :parents (rewrite))
        'rewrite)
    ;; The preset attaches the current buffer to `gptel-context'; we
    ;; inline the buffer with a <POINT/> marker into the user prompt
    ;; instead, so suppress the duplicate context.
    (let* ((gptel-context nil)
           (nosystem (gptel--model-capable-p 'nosystem))
           (gptel-use-context
            (and gptel-use-context (if nosystem 'user 'system)))
           (directive (bob/gptel-insert-directive))
           ;; Tell the model about the indentation of the line
           ;; containing <POINT/> so a bare docstring/statement comes
           ;; back with correct leading whitespace.
           (line-indent
            (save-excursion
              (beginning-of-line)
              (skip-chars-forward " \t")
              (buffer-substring-no-properties
               (line-beginning-position) (point))))
           (point-col (current-column))
           (buf-with-marker
            (concat (buffer-substring-no-properties (point-min) (point))
                    "<POINT/>"
                    (buffer-substring-no-properties (point) (point-max))))
           (user-prompt
            (format (concat "%s\n\n"
                            "Instruction: %s\n\n"
                            "The line containing <POINT/> starts with %d character(s) of indentation (%S) and <POINT/> itself is at column %d.  If the insertion spans multiple lines, indent subsequent lines to match.\n\n"
                            "Remember: output only the text to insert at <POINT/>.")
                    buf-with-marker instruction
                    (length line-indent) line-indent point-col))
           (prompt (list user-prompt))
           ;; Insert a single-char placeholder at point so the
           ;; overlay can span a real character.  Zero-width
           ;; overlays are invisible to `get-char-property', which
           ;; breaks upstream's dispatch/iterate/diff helpers.
           (placeholder-beg (point))
           (_ (insert bob/gptel-insert-placeholder))
           (placeholder-end (point))
           (ov (make-overlay placeholder-beg placeholder-end nil nil t)))
      ;; Tag the overlay so reject knows to delete the placeholder too.
      (overlay-put ov 'bob/gptel-insert-placeholder t)
      (overlay-put ov 'bob/gptel-insert-beg placeholder-beg)
      (overlay-put ov 'bob/gptel-insert-end placeholder-end)
      ;; Leave point at the placeholder start so the dispatch
      ;; transient finds the overlay on response.
      (goto-char placeholder-beg)
      (when nosystem
        (setcar prompt (concat directive "\n\n" (car prompt))))
      ;; Upstream's `gptel-request--transitions' table has no TPRE
      ;; state, so `gptel-pre-tool-call-functions' never fires.
      ;; `gptel-send--transitions' (which includes TPRE) is the table
      ;; chat uses; borrow it for our FSM so pre-tool hooks run.
      (let ((gptel-request--transitions
             (if (boundp 'gptel-send--transitions)
                 gptel-send--transitions
               gptel-request--transitions)))
        (gptel-request prompt
          :system directive
          :stream gptel-stream
          :context (cons ov (gptel--temp-buffer " *gptel-insert*"))
          :transforms gptel-prompt-transform-functions
          :fsm (gptel-make-fsm :handlers gptel--rewrite-handlers)
          :callback #'gptel--rewrite-callback)))))

;;; Installation ------------------------------------------------------

;;;###autoload
(defun bob/gptel-rewrite-install ()
  "Wire up the rewrite preset, advice and sanitiser.

Idempotent.  Safe to call from `use-package' `:config'."
  (with-eval-after-load 'gptel-rewrite
    ;; Pop the dispatch transient when a rewrite response arrives,
    ;; instead of leaving the overlay idle.
    (setq gptel-rewrite-default-action 'dispatch)
    ;; Register (or refresh) the preset.
    (gptel-make-preset 'rewrite
      :description "Region rewrites: Haiku, read-only tools, current buffer as context."
      :backend "Claude rewrite"
      :model 'claude-haiku-4-5-20251001
      :use-tools t
      :tools bob/gptel-rewrite-tools
      :use-context 'system
      ;; `:eval' runs when the preset is applied, inside a temp
      ;; buffer -- grab the originating buffer via the selected
      ;; window.
      :context '(:eval (let ((src (window-buffer (selected-window))))
                         (append gptel-context (list src)))))
    (unless (advice-member-p #'bob/gptel-rewrite-with-preset
                             'gptel--suffix-rewrite)
      (advice-add 'gptel--suffix-rewrite :around
                  #'bob/gptel-rewrite-with-preset))
    (unless (advice-member-p #'bob/gptel-rewrite-dispatch-fix-iterate
                             'gptel--rewrite-dispatch)
      (advice-add 'gptel--rewrite-dispatch :around
                  #'bob/gptel-rewrite-dispatch-fix-iterate))
    (unless (advice-member-p #'bob/gptel-rewrite-reject-cleans-insert-placeholder
                             'gptel--rewrite-reject)
      (advice-add 'gptel--rewrite-reject :before
                  #'bob/gptel-rewrite-reject-cleans-insert-placeholder))
    (unless (advice-member-p #'bob/gptel-rewrite-accept-cleans-overlay
                             'gptel--rewrite-accept)
      (advice-add 'gptel--rewrite-accept :after
                  #'bob/gptel-rewrite-accept-cleans-overlay)))
  (add-hook 'gptel-post-rewrite-functions
            #'bob/gptel-sanitize-llm-output))

(provide 'bob-gptel-rewrite)
;;; bob-gptel-rewrite.el ends here
