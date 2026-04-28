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
;; - `bob/gptel-rewrite': a thin wrapper around upstream
;;   `gptel-rewrite' that applies the `rewrite' preset automatically,
;;   without advising internal suffix functions.
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
;; Then bind `bob/gptel-rewrite' and `bob/gptel-insert' to convenient
;; keys (e.g. C-c g r / C-c g i).

;;; Code:

(require 'gptel)
(require 'cl-lib)
(require 'subr-x)

;; Forward declarations for gptel internals used intentionally here.
(defvar gptel--rewrite-overlays)
(defvar gptel--rewrite-handlers)
(defvar gptel--rewrite-message)
(defvar gptel-rewrite-default-action)

(declare-function gptel-make-anthropic "ext:gptel-anthropic" (name &rest args))
(declare-function gptel-rewrite "ext:gptel-rewrite" ())
(declare-function gptel--rewrite-reject "ext:gptel-rewrite" (&optional ovs))
(declare-function gptel--rewrite-update-status "ext:gptel-rewrite" (ov msg))
(declare-function gptel--rewrite-key-help "ext:gptel-rewrite" (&optional cb))
(declare-function gptel--rewrite-callback "ext:gptel-rewrite" (&rest args))
(declare-function gptel--suffix-rewrite "ext:gptel-rewrite" (&optional rewrite-message dry-run))

(declare-function org-element-at-point "org-element" (&optional epom cached-only))
(declare-function org-element-type "org-element" (node))
(declare-function org-element-property "org-element" (property element))

(defgroup bob-gptel-rewrite nil
  "Fast inline rewrite/insert commands for gptel."
  :group 'gptel)

;;; Backend ------------------------------------------------------------

(defvar bob/gptel-rewrite-backend
  (gptel-make-anthropic "Claude rewrite"
    :stream t
    ;; No adaptive thinking at the backend level -- Haiku
    ;; rewrites/inserts should stay fast.  16384 accommodates
    ;; the `C-u' path that enables thinking (see
    ;; `bob/gptel-insert'); Haiku won't emit anywhere near
    ;; this much on its own.  No server-side web/code tools
    ;; -- inline edits use the local read-only tool list
    ;; attached by the preset.
    :request-params '(:max_tokens 16384))
  "Non-thinking Anthropic backend used by the `rewrite' gptel preset.")

(defun bob/gptel-rewrite--pin-oauth-header ()
  "Pin OAuth headers on `bob/gptel-rewrite-backend'.

The `gptel-anthropic-oauth' module injects OAuth headers via
:around advice on `gptel-curl-get-response'.  That advice swaps
the header function on the dynamic `gptel-backend' variable --
but by the time the advice runs on a follow-up (after a tool
call), `gptel-backend' is the global default (\"Claude thinking\"),
NOT this rewrite backend.  Inside `gptel-curl-get-response' the
backend is then rebound to the FSM's backend (us), whose header
is never swapped, so the request goes out without x-api-key and
401s.

Pin a header function directly on this backend that always uses
`gptel-anthropic-oauth--get-oauth-headers'.  That makes auth work
on both the first request and every tool-result follow-up,
regardless of what `gptel-backend' points at.

Idempotent: re-pins on every call, which lets `bob/gptel-rewrite-install'
recover from an earlier load order where `gptel-anthropic-oauth' had
not defined the header function yet (the original `defvar' init form
is only evaluated once)."
  (when (and bob/gptel-rewrite-backend
             (fboundp 'gptel-anthropic-oauth--get-oauth-headers))
    (let ((b bob/gptel-rewrite-backend))
      (setf (gptel-backend-header b)
            (lambda (_info)
              (let ((gptel-backend b))
                (gptel-anthropic-oauth--get-oauth-headers)))))))

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

;;; Rewrite entry points ----------------------------------------------

(defvar bob/gptel-rewrite--source-buffer nil
  "Originating buffer for the current rewrite/insert request.

Dynamically bound by `bob/gptel-rewrite--with-preset'.  This avoids
relying on `selected-window' when the preset is applied, which is
fragile for minibuffers, transients, and special non-file buffers.")

(defun bob/gptel-rewrite--preset-context ()
  "Return context for the `rewrite' preset.

Always attaches the originating buffer object, not its file name, so
rewrite works for `*scratch*', gptel chat buffers, Magit/dired/help
buffers, unsaved buffers, and any other live buffer with a region.

Do not inherit ambient `gptel-context' here: regular chat sessions can
leave unrelated buffers/files in context, which makes small rewrite
models treat a simple instruction like \"upcase this\" as ambiguous."
  (let ((src (or (and (buffer-live-p bob/gptel-rewrite--source-buffer)
                      bob/gptel-rewrite--source-buffer)
                 (current-buffer))))
    (if (and (buffer-live-p src)
             (not (minibufferp src)))
        (list src)
      nil)))

(defun bob/gptel-rewrite--insert-overlay-at-point ()
  "Return the active `bob/gptel-insert' overlay at point, or nil."
  (cdr-safe (get-char-property-and-overlay
             (point) 'bob/gptel-insert-placeholder)))

(defconst bob/gptel-rewrite--source-buffer-syms
  '(gptel-backend gptel-model gptel-tools gptel-use-tools
    gptel-use-context gptel-context gptel-cache
    gptel-stream gptel-include-reasoning
    gptel--system-message gptel--request-params
    gptel-temperature gptel-max-tokens)
  "Gptel symbols that must reflect the rewrite preset in the source buffer.

Gptel's `gptel--with-buffer-copy' (used by `gptel-request' to build
the prompt buffer) copies these via `buffer-local-value', which
ignores `let'-bindings.  Without this list, applying the `rewrite'
preset via `gptel-with-preset' has no effect when the source buffer
is a `gptel-mode' chat buffer with buffer-local values for these
variables -- the chat buffer's Opus/thinking config wins and the
rewrite goes out as Opus instead of Haiku.

Must be a subset of the symbols in `gptel--with-buffer-copy-internal'.")

(defun bob/gptel-rewrite--existing-default-directory (&optional buffer)
  "Return a usable `default-directory' for BUFFER or the current buffer.

Non-file/special buffers can retain a `default-directory' from a
worktree or project directory that has since been deleted.  Gptel and
its tools may then error with \"Setting current directory: No such file
or directory\" before the request is even sent."
  (with-current-buffer (or buffer (current-buffer))
    (file-name-as-directory
     (expand-file-name
      (or (and buffer-file-name
               (file-directory-p (file-name-directory buffer-file-name))
               (file-name-directory buffer-file-name))
          (and (stringp default-directory)
               (file-directory-p default-directory)
               default-directory)
          (and (fboundp 'project-current)
               (when-let* ((project (ignore-errors (project-current nil)))
                           (root (ignore-errors (project-root project)))
                           ((file-directory-p root)))
                 root))
          user-emacs-directory
          "~/")))))

(defun bob/gptel-rewrite--with-source-buffer-overrides (thunk)
  "Run THUNK with rewrite-preset values pushed into the source buffer.

For each symbol in `bob/gptel-rewrite--source-buffer-syms', if the
source buffer has a buffer-local binding, save it and overwrite it
with the current dynamic value (which `gptel-with-preset' has set).
Also temporarily repairs a stale/nonexistent `default-directory'.

On unwind, restore the original buffer-local bindings -- including
the distinction between `was buffer-local' and `had no buffer-local
binding'.  If THUNK kicks off an async request that outlives this
call, the request has already captured the preset values via
`buffer-local-value', so the restore is safe."
  (let ((buf (current-buffer))
        (saved nil)
        (old-default-directory default-directory))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (setq default-directory
                  (bob/gptel-rewrite--existing-default-directory buf)))
          (dolist (sym bob/gptel-rewrite--source-buffer-syms)
            (when (boundp sym)
              (let ((had-local (local-variable-p sym buf))
                    (old-local (and (local-variable-p sym buf)
                                    (buffer-local-value sym buf)))
                    (new-val (symbol-value sym))) ;dynamic value
                (push (list sym had-local old-local) saved)
                (with-current-buffer buf
                  (set (make-local-variable sym) new-val)))))
          (funcall thunk))
      (with-current-buffer buf
        (setq default-directory old-default-directory)
        (dolist (entry saved)
          (let ((sym (nth 0 entry))
                (had-local (nth 1 entry))
                (old-local (nth 2 entry)))
            (if had-local
                (set (make-local-variable sym) old-local)
              (kill-local-variable sym))))))))

(defun bob/gptel-rewrite--with-preset (thunk)
  "Run THUNK with the `rewrite' preset and rewrite FSM transitions."
  ;; Work around upstream oversight: `gptel--rewrite-handlers'
  ;; defines a `TPRE' handler for `gptel-pre-tool-call-functions',
  ;; but the default `gptel-request--transitions' table never enters
  ;; `TPRE'.  Rebind to `gptel-send--transitions' (which includes
  ;; TPRE) while constructing the rewrite FSM so pre-tool-call hooks
  ;; fire as intended.
  (let ((gptel-request--transitions
         (if (boundp 'gptel-send--transitions)
             gptel-send--transitions
           gptel-request--transitions))
        (bob/gptel-rewrite--source-buffer (current-buffer)))
    (gptel-with-preset 'rewrite
      ;; `gptel-with-preset' uses dynamic `let'-bindings, but gptel
      ;; later builds the prompt buffer via `gptel--with-buffer-copy'
      ;; which calls `buffer-local-value' on the source buffer for
      ;; `gptel-backend', `gptel-model' etc.  `buffer-local-value'
      ;; ignores `let'-bindings, so when the source buffer is a
      ;; chat buffer with buffer-local Opus/thinking config, the
      ;; preset is silently overridden.  Push the preset's values
      ;; into the source buffer's locals for the duration of the
      ;; request setup so the copy picks them up correctly.
      (bob/gptel-rewrite--with-source-buffer-overrides thunk))))

(defun bob/gptel-rewrite--iterate-insert-overlay (ov &optional prompt)
  "Reject insert overlay OV and launch a fresh insert iteration.

PROMPT is the minibuffer prompt for the new instruction."
  (let* ((beg (overlay-get ov 'bob/gptel-insert-beg))
         (prior-instruction
          (overlay-get ov 'bob/gptel-insert-instruction))
         (prior-response (overlay-get ov 'gptel-rewrite))
         (prior-strong (overlay-get ov 'bob/gptel-insert-strong))
         (instruction (read-string (or prompt "Iterate insert: ")
                                   nil nil prior-instruction)))
    (goto-char beg)
    (gptel--rewrite-reject ov)
    (bob/gptel-insert instruction prior-strong
                      (list :instruction prior-instruction
                            :response prior-response))))

(defun bob/gptel-rewrite-default-action (ov)
  "Custom `gptel-rewrite-default-action' implementation for OV.

This mirrors upstream dispatch UX, but fixes the non-interactive
`iterate' branch and preserves insert semantics when iterating an
overlay created by `bob/gptel-insert'."
  (let* ((orig-status (copy-sequence (overlay-get ov 'status)))
         (choices '((?a "accept") (?k "reject") (?r "iterate")
                    (?m "merge") (?d "diff") (?e "ediff")))
         (choice
          (unwind-protect
              (progn
                (gptel--rewrite-update-status
                 ov (when (fboundp 'rmc--add-key-description)
                      (concat " "
                              (mapconcat #'cdr
                                         (mapcar #'rmc--add-key-description
                                                 choices)
                                         ", "))))
                (read-multiple-choice "Action: " choices))
            (overlay-put ov 'status orig-status)
            (overlay-put ov 'before-string
                         (apply #'concat orig-status))))
         (action (cadr choice)))
    (cond
     ((and (string= action "iterate")
           (overlay-get ov 'bob/gptel-insert-placeholder))
      (bob/gptel-rewrite--iterate-insert-overlay ov))
     ((string= action "iterate")
      (goto-char (overlay-start ov))
      (call-interactively #'bob/gptel-rewrite))
     (t
      (funcall (intern (concat "gptel--rewrite-" action)) ov)))))

(defvar-local bob/gptel-rewrite--captured-region nil
  "Captured source region for the next upstream rewrite suffix call.

Value is a cons of markers (BEG . END).  It is set before entering
upstream's rewrite minibuffer, then consumed by
`bob/gptel-rewrite--suffix-restores-captured-region'.  Keeping this
buffer-local lets upstream's `M-RET' transient path survive outside the
original dynamic extent.")

(defun bob/gptel-rewrite--capture-region ()
  "Capture the current active region for a later upstream suffix call."
  (when-let* (((use-region-p))
              (old bob/gptel-rewrite--captured-region))
    (set-marker (car old) nil)
    (set-marker (cdr old) nil))
  (setq bob/gptel-rewrite--captured-region
        (cons (copy-marker (region-beginning))
              (copy-marker (region-end) t))))

(defun bob/gptel-rewrite--clear-captured-region (&optional buffer)
  "Clear captured rewrite-region markers in BUFFER or current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (when bob/gptel-rewrite--captured-region
      (set-marker (car bob/gptel-rewrite--captured-region) nil)
      (set-marker (cdr bob/gptel-rewrite--captured-region) nil)
      (setq bob/gptel-rewrite--captured-region nil))))

(defun bob/gptel-rewrite--valid-captured-region-p ()
  "Non-nil if `bob/gptel-rewrite--captured-region' is usable."
  (and-let* ((region bob/gptel-rewrite--captured-region)
             (beg (car region))
             (end (cdr region))
             (buf (marker-buffer beg)))
    (and (buffer-live-p buf)
         (eq buf (marker-buffer end))
         (marker-position beg)
         (marker-position end))))

(defun bob/gptel-rewrite--current-overlay ()
  "Return the rewrite overlay covering point, or nil.

Upstream's `gptel--suffix-rewrite' uses the same probe to decide whether
this is a fresh rewrite or an iterate; we use it to decide whether to
thread iteration history into the prompt."
  (cdr-safe (get-char-property-and-overlay (point) 'gptel-rewrite)))

(defun bob/gptel-rewrite--iterate-prompt (overlay current-instruction)
  "Build a prompt list for an iterate request on OVERLAY.

The returned list interleaves prior instructions and responses so the
LLM sees the full edit history for this rewrite, ending with the
current target text and CURRENT-INSTRUCTION.  Falls back to nil when
the overlay carries no captured original target -- the caller should
then leave upstream's prompt construction alone."
  (when-let* ((overlay overlay)
              (original (overlay-get overlay 'bob/gptel-rewrite-original))
              (history (overlay-get overlay 'bob/gptel-rewrite-history))
              (current-target (or (overlay-get overlay 'gptel-rewrite)
                                  original))
              (request-line
               "What is the required change?  I will generate only the final replacement."))
    (let ((prompt (list original request-line)))
      (dolist (entry history)
        (let ((instr (car entry))
              (resp (cdr entry)))
          (when (and (stringp instr) (stringp resp))
            (setq prompt (append prompt (list instr resp request-line))))))
      (append prompt (list current-target current-instruction)))))

(defvar-local bob/gptel-rewrite--pending-instruction nil
  "Instruction sent for the most recent rewrite request in this buffer.

Captured by `bob/gptel-rewrite--suffix-restores-captured-region' so
`bob/gptel-rewrite--record-history' can append it to the resulting
overlay's history when the response arrives.")

(defvar-local bob/gptel-rewrite--pending-overlay nil
  "Rewrite overlay associated with the most recent rewrite request.")

(defun bob/gptel-rewrite--ensure-overlay-history (overlay original)
  "Initialise iteration metadata on OVERLAY if missing.

ORIGINAL is the originally selected text.  Idempotent: already-set
properties are preserved across iterate cycles."
  (unless (overlay-get overlay 'bob/gptel-rewrite-original)
    (overlay-put overlay 'bob/gptel-rewrite-original original))
  (unless (overlay-get overlay 'bob/gptel-rewrite-history)
    (overlay-put overlay 'bob/gptel-rewrite-history nil)))

(defun bob/gptel-rewrite--record-history (_beg _end)
  "Append the latest (instruction . response) to the rewrite overlay.

Runs from `gptel-post-rewrite-functions' inside the rewrite proc-buffer,
at which point the proc-buffer's contents are the new response and the
source overlay still lives on `bob/gptel-rewrite--pending-overlay' in
the originating buffer."
  (let* ((proc-buf (current-buffer))
         (response (buffer-substring-no-properties (point-min) (point-max)))
         (src-buf (cl-some (lambda (buf)
                             (with-current-buffer buf
                               (and bob/gptel-rewrite--pending-overlay
                                    (overlay-buffer
                                     bob/gptel-rewrite--pending-overlay)
                                    buf)))
                           (buffer-list))))
    (when src-buf
      (with-current-buffer src-buf
        (let ((overlay bob/gptel-rewrite--pending-overlay)
              (instruction bob/gptel-rewrite--pending-instruction))
          (when (and (overlayp overlay)
                     (overlay-buffer overlay)
                     (stringp instruction)
                     (not (string-empty-p (string-trim instruction)))
                     (stringp response)
                     (not (string-empty-p response)))
            (let ((history (overlay-get overlay 'bob/gptel-rewrite-history)))
              (overlay-put overlay 'bob/gptel-rewrite-history
                           (append history
                                   (list (cons instruction response)))))))
        (setq bob/gptel-rewrite--pending-instruction nil
              bob/gptel-rewrite--pending-overlay nil)))
    (ignore proc-buf)))

(defun bob/gptel-rewrite--suffix-restores-captured-region (orig &rest args)
  "Advice around `gptel--suffix-rewrite' to restore region and thread history.

Responsibilities:

1. Restore markers captured by `bob/gptel-rewrite' so a deactivated
   source region or escaped minibuffer mark state cannot make the
   request lose its target text.
2. Reapply the `rewrite' preset at suffix time so the upstream `M-RET'
   transient path -- which is scheduled outside the original dynamic
   extent of `bob/gptel-rewrite--with-preset' -- still uses Haiku and
   the rewrite backend instead of inheriting the chat buffer's config.
3. Thread iteration history.  For iterate calls, replace upstream's
   single-shot prompt list with one that includes the original target
   plus every (instruction . response) recorded on the overlay so far,
   and remember the current instruction + overlay so the
   post-rewrite hook can append the new pair."
  (let ((have-region (bob/gptel-rewrite--valid-captured-region-p)))
    (cl-flet ((run ()
                (let* ((overlay (bob/gptel-rewrite--current-overlay))
                       (instruction (or (car args)
                                        gptel--rewrite-message))
                       (iterate-prompt
                        (and overlay (stringp instruction)
                             (bob/gptel-rewrite--iterate-prompt
                              overlay instruction)))
                       (orig-request (symbol-function 'gptel-request)))
                  ;; Capture metadata so the post-rewrite hook can
                  ;; finalise the history entry.  Done in the source
                  ;; buffer (current-buffer here is the source).
                  (setq bob/gptel-rewrite--pending-instruction instruction
                        bob/gptel-rewrite--pending-overlay nil)
                  (cl-letf (((symbol-function 'gptel-request)
                             (lambda (&rest req-args)
                               (let* ((req-prompt (car req-args))
                                      (req-rest (cdr req-args))
                                      (req-context (plist-get req-rest :context))
                                      (req-overlay (car-safe req-context))
                                      (final-prompt
                                       (or iterate-prompt req-prompt))
                                      (orig-text
                                        (cond
                                         ((stringp req-prompt) req-prompt)
                                         ((consp req-prompt) (car req-prompt))
                                         (t nil))))
                                 (when (and (overlayp req-overlay) orig-text)
                                   (bob/gptel-rewrite--ensure-overlay-history
                                    req-overlay orig-text)
                                   (setq bob/gptel-rewrite--pending-overlay
                                         req-overlay))
                                 (apply orig-request final-prompt req-rest)))))
                    (apply orig args)))))
      (if (not have-region)
          (run)
        (let* ((region bob/gptel-rewrite--captured-region)
               (beg (car region))
               (end (cdr region))
               (buf (marker-buffer beg)))
          (unwind-protect
              (with-current-buffer buf
                (let ((deactivate-mark nil))
                  (goto-char beg)
                  (push-mark end t t)
                  (activate-mark)
                  (bob/gptel-rewrite--with-preset #'run)))
            (bob/gptel-rewrite--clear-captured-region buf)))))))

;;;###autoload
(defun bob/gptel-rewrite ()
  "Run `gptel-rewrite' under the bob `rewrite' preset.

If point is on a pending `bob/gptel-insert' overlay, iterate that
insert instead of rewriting its generated output.

For a fresh active-region rewrite, snapshot the region before handing
off to upstream so minibuffer mark/cancel behavior cannot make the
rewrite request lose its target text.  Upstream's prompt, history,
`M-RET' transient, dry-run inspection, and pending-overlay UX remain
intact."
  (interactive)
  (require 'gptel-rewrite)
  (cond
   ((bob/gptel-rewrite--insert-overlay-at-point)
    (bob/gptel-rewrite--iterate-insert-overlay
     (bob/gptel-rewrite--insert-overlay-at-point) "Insert: "))
   (t
    (when (use-region-p)
      (bob/gptel-rewrite--capture-region))
    ;; Suppress upstream's "Rewrite: " placeholder default.  When
    ;; `gptel--rewrite-message' is nil upstream uses that string as
    ;; the initial editable input, which then bleeds into the user
    ;; instruction unless the placeholder is overwritten.  Bind to an
    ;; empty string so the minibuffer comes up empty for fresh
    ;; rewrites; iterate paths set their own message before
    ;; re-prompting and are unaffected.
    (let ((gptel--rewrite-message (or gptel--rewrite-message "")))
      (bob/gptel-rewrite--with-preset
       (lambda () (call-interactively #'gptel-rewrite)))))))

;;; Rewrite directive -------------------------------------------------

(defun bob/gptel-rewrite-directive ()
  "Return a stricter directive for `gptel-rewrite'."
  (let* ((lang (bob/gptel-rewrite-mode-language))
         (article (if (and lang (not (string-empty-p lang))
                           (memq (aref lang 0) '(?a ?e ?i ?o ?u)))
                      "an" "a")))
    (concat
     (if (string-empty-p lang)
         "You are an editor."
       (format "You are %s %s editor." article lang))
     " Rewrite exactly the target text supplied by the user.\n"
     "The FIRST user message is the exact text to replace.\n"
     "The later instruction says how to transform that target.\n"
     "Any provided buffer/context is only for style and disambiguation; do not rewrite it as a whole.\n"
     "If the instruction says 'this', it refers to the first user message.\n"
     "Output ONLY the replacement text, with no explanation, no choices, and no markdown fences.")))

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

(defconst bob/gptel-rewrite-nonprogramming-languages
  '("json" "yaml" "toml" "markdown")
  "Language labels treated as non-programming for insert directives.")

(defconst bob/gptel-rewrite-nonprogramming-extensions
  '("json" "yaml" "yml" "toml" "md")
  "File extensions treated as non-programming for insert directives.")

(defun bob/gptel-rewrite--programming-extension-p (ext)
  "Return non-nil when EXT looks like a source-code extension."
  (let ((e (downcase (or ext ""))))
    (and (not (string-empty-p e))
         (assoc e bob/gptel-rewrite-extension-lang-alist)
         (not (member e bob/gptel-rewrite-nonprogramming-extensions)))))

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
fails, but avoids classifying data/prose formats (JSON/YAML/TOML/
Markdown) as code."
  (or
   ;; Org src blocks are code even inside prose documents.
   (and (derived-mode-p 'org-mode)
        (fboundp 'org-element-at-point)
        (let ((el (ignore-errors (org-element-at-point))))
          (and el (eq (org-element-type el) 'src-block))))
   (let ((lang (bob/gptel-rewrite-mode-language)))
     (and (not (member lang bob/gptel-rewrite-nonprogramming-languages))
          (or (derived-mode-p 'prog-mode)
              (string-suffix-p "-ts-mode" (symbol-name major-mode))
              ;; File extension known to be source code -- catches
              ;; buffers in `fundamental-mode' over a .rs file, etc.
              (when-let* ((f (buffer-file-name))
                          (ext (file-name-extension f)))
                (bob/gptel-rewrite--programming-extension-p ext)))))))

;;; Insert: directive --------------------------------------------------

(defconst bob/gptel-insert-directive-programming-template
  (concat
   "You are %s %s programmer. Output only text to insert at <POINT/>.\n"
   "The full buffer with <POINT/> is already in the user prompt; do not search for it.\n"
   "Use tools only if the instruction explicitly needs information from other files.\n"
   "Rules:\n"
   "- Output ONLY the insertion text (no fences, no prose, no commentary).\n"
   "- Do not repeat surrounding file content.\n"
   "- Match local style, naming, and indentation.\n"
   "- If ambiguous, make the most reasonable assumption and still produce insertion text.\n"
   "- If request implies rewrites around <POINT/>, output only minimal NEW text to insert.\n"
   "Example: if asked to add a docstring at <POINT/>, output just: \"Return the square of X.\"")
  "Template for programming-buffer insert directive.")

(defun bob/gptel-insert-directive ()
  "Return the system directive for `bob/gptel-insert'."
  (let* ((lang (bob/gptel-rewrite-mode-language))
         (article (if (and lang (not (string-empty-p lang))
                           (memq (aref lang 0) '(?a ?e ?i ?o ?u)))
                      "an" "a")))
    (if (bob/gptel-rewrite-mode-is-programmingish)
        (format bob/gptel-insert-directive-programming-template
                article lang)
      (concat
       (if (string-empty-p lang)
           "You are an editor."
         (format "You are %s %s editor." article lang))
       " Output only text to insert at <POINT/>."
       " No markdown fences, no commentary, no explanation."
       " Do not repeat surrounding content; match local style."
       " If ambiguous, make a reasonable assumption and produce insertion text anyway."))))

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

(defcustom bob/gptel-sanitize-llm-output-style 'conservative
  "How aggressively to sanitize rewrite/insert LLM output.

`conservative' (default):
- unwrap only when the ENTIRE output is exactly one fenced block
  (plus surrounding whitespace), then trim surrounding blank lines.

`aggressive':
- old behavior: if any fenced block exists, keep the body of the
  last one, then trim surrounding blank lines.

`off':
- disable sanitizer entirely."
  :type '(choice (const :tag "Conservative" conservative)
                 (const :tag "Aggressive" aggressive)
                 (const :tag "Off" off))
  :group 'bob-gptel-rewrite)

(defun bob/gptel-sanitize--single-fenced-body ()
  "Return body text if narrowed buffer is exactly one fenced block.

Allows leading/trailing whitespace outside fences.  Returns nil
when output includes additional non-whitespace prose or multiple
fenced blocks."
  (save-excursion
    (goto-char (point-min))
    (skip-chars-forward " \t\n")
    (when (looking-at "[ \t]*```[^\n]*\n")
      (let ((body-start (match-end 0)))
        (when (re-search-forward "^[ \t]*```[ \t]*$" nil t)
          (let ((body-end (match-beginning 0))
                (after-end (match-end 0)))
            (goto-char after-end)
            (skip-chars-forward " \t\n")
            (when (= (point) (point-max))
              (buffer-substring-no-properties body-start body-end))))))))

(defun bob/gptel-sanitize-llm-output (beg end)
  "Sanitize LLM output in region BEG..END (rewrite/insert temp buffer).

Runs from `gptel-post-rewrite-functions' in the rewrite temp
buffer, before the overlay is populated.  By default this is
intentionally conservative to avoid clobbering legitimate
non-code rewrites."
  (unless (eq bob/gptel-sanitize-llm-output-style 'off)
    (save-excursion
      (save-restriction
        (narrow-to-region beg end)
        (pcase bob/gptel-sanitize-llm-output-style
          ('conservative
           (when-let* ((body (bob/gptel-sanitize--single-fenced-body)))
             (delete-region (point-min) (point-max))
             (insert body)))
          ('aggressive
           (let ((bodies (bob/gptel-sanitize--all-fence-bodies)))
             (when bodies
               (let* ((last (car (last bodies)))
                      (body (buffer-substring-no-properties
                             (car last) (cdr last))))
                 (delete-region (point-min) (point-max))
                 (insert body))))))
        ;; Trim surrounding blank lines.
        (goto-char (point-min))
        (when (looking-at "[ \t]*\n+")
          (replace-match ""))
        (goto-char (point-max))
        (when (re-search-backward "\n[ \t\n]+\\'" nil t)
          (replace-match "\n"))))))

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
(defun bob/gptel-insert (instruction &optional strong prior)
  "Insert LLM-generated text at point, driven by INSTRUCTION.

Shows the same accept/reject/diff/iterate overlay UX as
`gptel-rewrite'.  The full buffer is sent with a <POINT/> marker
so the model knows exactly where the insertion goes.

By default uses Haiku 4.5 with NO tools: the buffer is already
in the prompt, so context-discovery tools (read/grep/find/ls)
tend to confuse small models into reading random paths and
asking \"where is <POINT/>?\".

With a prefix argument STRONG, upgrade to Claude Sonnet 4.6 and
re-enable the rewrite preset's read-only tool list -- useful
for non-trivial inserts that genuinely need to look at sibling
files.

PRIOR, when non-nil, is a plist of iterate context from a
previous `bob/gptel-insert' whose response the user wants to
refine.  Keys:
  :instruction  -- the earlier instruction.
  :response     -- the text the LLM produced last time.
It is appended to the user prompt so the model can refine the
previous answer instead of starting from scratch (which would
be confused by a buffer that no longer contains the prior
response)."
  (interactive (list (read-string (if current-prefix-arg
                                      "Insert (Sonnet + tools): "
                                    "Insert: "))
                     current-prefix-arg))
  (when (string-empty-p (string-trim instruction))
    (user-error "Empty instruction"))
  ;; We reuse the rewrite FSM handlers and callback.
  (require 'gptel-rewrite)
  ;; Presets:
  ;; - Default: parent is `rewrite', but override :tools/:use-tools
  ;;   to turn tools OFF.  The full buffer is in the prompt as
  ;;   <POINT/>...</POINT/>, so context-discovery tools are noise
  ;;   that Haiku invariably abuses (reads random paths, greps the
  ;;   home directory, ends up asking "where is <POINT/>?").
  ;; - With C-u: upgrade to Sonnet 4.6 with adaptive thinking AND
  ;;   keep the rewrite preset's tool list, for cross-file inserts
  ;;   that genuinely need to look around.  `:request-params' here
  ;;   overrides the "Claude rewrite" backend's non-thinking
  ;;   defaults (see `bob/gptel-rewrite-backend').
  (gptel-with-preset
      (if strong
          ;; Sonnet 4.6 + adaptive thinking for hard inserts.  Tools
          ;; inherit from the parent `rewrite' preset (read/ls/find/grep).
          ;; `:max_tokens' comes from the "Claude rewrite" backend
          ;; (16384), which is plenty for insert output plus thinking.
          '(:model claude-sonnet-4-6
            :parents (rewrite)
            :request-params (:thinking (:type "adaptive"
                                        :display "summarized")))
        '(:parents (rewrite) :use-tools nil :tools nil))
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
           (prior-section
            (when prior
              (format (concat "\n\nPREVIOUS ITERATION\n"
                              "Earlier instruction: %s\n"
                              "You previously produced this text (not in the buffer any more):\n"
                              "<PREVIOUS-OUTPUT>\n%s\n</PREVIOUS-OUTPUT>\n\n"
                              "The user has now refined the instruction.  Treat the new instruction below as a modification of the previous one, with the previous output as a starting point.  Produce the NEW final text to insert at <POINT/>.")
                      (or (plist-get prior :instruction) "(unknown)")
                      (or (plist-get prior :response) "(unknown)"))))
           (user-prompt
            (format (concat "%s\n\n"
                            "Instruction: %s%s\n\n"
                            "The line containing <POINT/> starts with %d character(s) of indentation (%S) and <POINT/> itself is at column %d.  If the insertion spans multiple lines, indent subsequent lines to match.\n\n"
                            "Remember: output only the text to insert at <POINT/>.")
                    buf-with-marker instruction (or prior-section "")
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
      ;; Tag the overlay so reject knows to delete the placeholder too
      ;; and so iterate can recover the instruction for refinement.
      (overlay-put ov 'bob/gptel-insert-placeholder t)
      (overlay-put ov 'bob/gptel-insert-beg placeholder-beg)
      (overlay-put ov 'bob/gptel-insert-end placeholder-end)
      (overlay-put ov 'bob/gptel-insert-instruction instruction)
      (overlay-put ov 'bob/gptel-insert-strong strong)
      ;; Register the overlay with the activity module so it gets
      ;; the in-place spinner next to the insertion point.  Cleared
      ;; automatically when the response arrives (pre-response hook
      ;; wipes it via `bob/gptel-activity--clear-all-overlays').
      (when (fboundp 'bob/gptel-activity-track-overlay)
        (bob/gptel-activity-track-overlay ov))
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
        ;; Push the preset's dynamic values into the source buffer's
        ;; locals: gptel's prompt-buffer copy uses `buffer-local-value'
        ;; which ignores `let'-bindings, so without this the chat
        ;; buffer's Opus/thinking config silently overrides the preset.
        ;; See `bob/gptel-rewrite--with-source-buffer-overrides'.
        (bob/gptel-rewrite--with-source-buffer-overrides
         (lambda ()
           (gptel-request prompt
             :system directive
             :stream gptel-stream
             :context (cons ov (gptel--temp-buffer " *gptel-insert*"))
             :transforms gptel-prompt-transform-functions
             :fsm (gptel-make-fsm :handlers gptel--rewrite-handlers)
             :callback #'gptel--rewrite-callback)))))))

;;; Installation ------------------------------------------------------

;;;###autoload
(defun bob/gptel-rewrite-install ()
  "Wire up rewrite preset, default action, lifecycle cleanup and sanitiser.

Idempotent.  Safe to call from `use-package' `:config'."
  ;; Pin OAuth header on the rewrite backend.  This must run after
  ;; `gptel-anthropic-oauth' has been loaded; doing it here (rather
  ;; than at file load time) sidesteps a load-order race where the
  ;; OAuth header function was not yet `fboundp' when the rewrite
  ;; backend was constructed -- in which case the backend silently
  ;; falls back to the default `x-api-key' header and 401s.
  (bob/gptel-rewrite--pin-oauth-header)
  (with-eval-after-load 'gptel-anthropic-oauth
    (bob/gptel-rewrite--pin-oauth-header))
  (with-eval-after-load 'gptel-rewrite
    ;; Register (or refresh) the preset.
    (gptel-make-preset 'rewrite
      :description "Region rewrites: Haiku, read-only tools, current buffer as context."
      :backend "Claude rewrite"
      :model 'claude-haiku-4-5-20251001
      :use-tools t
      :tools bob/gptel-rewrite-tools
      :use-context 'system
      ;; Attach the originating buffer as a buffer object, not as a
      ;; file path, so rewrite works in special/unsaved/non-file
      ;; buffers too.  `bob/gptel-rewrite--source-buffer' is bound by
      ;; `bob/gptel-rewrite--with-preset'; fall back to current buffer
      ;; when the preset is applied manually.
      :context '(:eval (bob/gptel-rewrite--preset-context))
      :rewrite-directive #'bob/gptel-rewrite-directive)
    ;; Use our custom dispatch as the default post-response action.
    (setq gptel-rewrite-default-action #'bob/gptel-rewrite-default-action)
    ;; Remove legacy internal advice if it exists from older versions.
    (when (advice-member-p 'bob/gptel-rewrite-with-preset
                           'gptel--suffix-rewrite)
      (advice-remove 'gptel--suffix-rewrite 'bob/gptel-rewrite-with-preset))
    (when (advice-member-p 'bob/gptel-rewrite-dispatch-fix-iterate
                           'gptel--rewrite-dispatch)
      (advice-remove 'gptel--rewrite-dispatch 'bob/gptel-rewrite-dispatch-fix-iterate))
    ;; Keep only lifecycle cleanup advice not covered by public hooks.
    (unless (advice-member-p #'bob/gptel-rewrite-reject-cleans-insert-placeholder
                             'gptel--rewrite-reject)
      (advice-add 'gptel--rewrite-reject :before
                  #'bob/gptel-rewrite-reject-cleans-insert-placeholder))
    (unless (advice-member-p #'bob/gptel-rewrite-accept-cleans-overlay
                             'gptel--rewrite-accept)
      (advice-add 'gptel--rewrite-accept :after
                  #'bob/gptel-rewrite-accept-cleans-overlay))
    (unless (advice-member-p #'bob/gptel-rewrite--suffix-restores-captured-region
                             'gptel--suffix-rewrite)
      (advice-add 'gptel--suffix-rewrite :around
                  #'bob/gptel-rewrite--suffix-restores-captured-region)))
  (add-hook 'gptel-post-rewrite-functions
            #'bob/gptel-sanitize-llm-output)
  ;; Append (instruction . response) to the rewrite overlay's history
  ;; once the response is ready.  Run AFTER the sanitiser so we record
  ;; the cleaned-up response that the overlay actually shows.
  (add-hook 'gptel-post-rewrite-functions
            #'bob/gptel-rewrite--record-history t))

(provide 'bob-gptel-rewrite)
;;; bob-gptel-rewrite.el ends here
