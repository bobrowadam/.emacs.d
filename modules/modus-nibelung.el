;;; modus-nibelung.el --- Shared modus-nibelung Modus definitions -*- lexical-binding: t; -*-

(require 'modus-themes)

(defconst modus-nibelung-light-palette
  (modus-themes-generate-palette
   '((bg-main "#F8F9FA")
     (bg-dim "#E9ECEF")
     (bg-alt "#E9ECEF")
     (bg-active "#DEE2E6")
     (bg-inactive "#E9ECEF")
     (bg-hover "#E2EAFC")
     (bg-hover-secondary "#DEE2E6")
     (bg-added "#E4F0E7")
     (bg-added-faint "#EEF5F0")
     (bg-added-refine "#CFE3D4")
     (bg-added-fringe "#6F9B7C")
     (fg-added "#355C41")
     (fg-added-intense "#244C32")
     (bg-removed "#F2E3E3")
     (bg-removed-faint "#F7EEEE")
     (bg-removed-refine "#E5CACA")
     (bg-removed-fringe "#B57474")
     (fg-removed "#7B3F3F")
     (fg-removed-intense "#652F2F")
     (fg-main "#495057")
     (fg-dim "#6C757D")
     (fg-alt "#ADB5BD")
     (red "#E08E8E")
     (green "#8FBF9F")
     (yellow "#C9C97D")
     (blue "#7B8CDE")
     (magenta "#C99BC9")
     (cyan "#7EB8B8")
     (modus-nibelung-level-0 "#E9ECEF")
     (modus-nibelung-level-1 "#DEE2E6")
     (modus-nibelung-level-2 "#CED4DA")
     (modus-nibelung-level-3 "#ADB5BD")
     (modus-nibelung-level-4 "#6C757D")
     (modus-nibelung-level-5 "#343A40")
     (modus-nibelung-level-6 "#212529")
     (modus-nibelung-accent-subtle "#E2EAFC")
     (modus-nibelung-accent-light "#B6CCFE")
     (modus-nibelung-accent "#9BB1FF"))
   'cool)
  "Complete Modus palette for the light modus-nibelung theme.")

(defconst modus-nibelung-dark-palette
  (modus-themes-generate-palette
   '((bg-main "#212529")
     (bg-dim "#2B3035")
     (bg-alt "#2B3035")
     (bg-active "#343A40")
     (bg-inactive "#2B3035")
     (bg-hover "#2B375C")
     (bg-hover-secondary "#343A40")
     (bg-added "#27382F")
     (bg-added-faint "#232D27")
     (bg-added-refine "#355744")
     (bg-added-fringe "#4F765B")
     (fg-added "#CAFFBF")
     (fg-added-intense "#CAFFBF")
     (bg-removed "#3D292B")
     (bg-removed-faint "#302426")
     (bg-removed-refine "#63383C")
     (bg-removed-fringe "#9B5C5C")
     (fg-removed "#FFADAD")
     (fg-removed-intense "#FFADAD")
     (fg-main "#CED4DA")
     (fg-dim "#ADB5BD")
     (fg-alt "#6C757D")
     (red "#FFADAD")
     (green "#CAFFBF")
     (yellow "#FDFFB6")
     (blue "#9FA0FF")
     (magenta "#FFC6FF")
     (cyan "#9BF6FF")
     (modus-nibelung-level-0 "#2B3035")
     (modus-nibelung-level-1 "#343A40")
     (modus-nibelung-level-2 "#495057")
     (modus-nibelung-level-3 "#6C757D")
     (modus-nibelung-level-4 "#ADB5BD")
     (modus-nibelung-level-5 "#DEE2E6")
     (modus-nibelung-level-6 "#F8F9FA")
     (modus-nibelung-accent-subtle "#2B375C")
     (modus-nibelung-accent-light "#577DB7")
     (modus-nibelung-accent "#9BB1FF"))
   'cool)
  "Complete Modus palette for the dark modus-nibelung theme.")

(defcustom modus-nibelung-light-palette-overrides nil
  "Palette overrides for the light modus-nibelung theme."
  :group 'modus-themes
  :type '(repeat (list symbol (choice symbol string))))

(defcustom modus-nibelung-dark-palette-overrides nil
  "Palette overrides for the dark modus-nibelung theme."
  :group 'modus-themes
  :type '(repeat (list symbol (choice symbol string))))

(defconst modus-nibelung-custom-faces
  '(
    ;; Preserve modus-nibelung's restrained typography and syntax palette.
    `(bold ((,c :foreground ,modus-nibelung-accent :weight normal)))
    `(bold-italic ((,c :foreground ,modus-nibelung-accent :weight normal)))
    `(italic ((,c :foreground ,modus-nibelung-accent)))
    `(font-lock-builtin-face ((,c :foreground ,fg-dim :weight normal)))
    `(font-lock-preprocessor-face ((,c :foreground ,fg-dim :weight normal)))
    `(font-lock-constant-face ((,c :foreground ,fg-dim :weight normal)))
    `(font-lock-function-call-face ((,c :foreground ,modus-nibelung-accent :weight normal)))
    `(font-lock-function-name-face ((,c :foreground ,fg-main :weight normal)))
    `(font-lock-keyword-face ((,c :foreground ,fg-dim :weight normal)))
    `(font-lock-string-face ((,c :foreground ,fg-main :weight normal)))
    `(font-lock-type-face ((,c :foreground ,fg-dim :weight normal)))
    `(font-lock-variable-name-face ((,c :foreground ,fg-dim :weight normal)))
    `(font-lock-comment-face
      ((,c :background ,modus-nibelung-level-0 :foreground ,fg-dim :extend t)))
    `(font-lock-comment-delimiter-face
      ((,c :background ,modus-nibelung-level-0 :foreground ,fg-dim :extend t)))
    `(font-lock-doc-face ((,c :foreground ,fg-dim)))
    `(shadow ((,c :foreground ,modus-nibelung-level-3)))

    ;; Window chrome.  Spacious Padding reads these faces after each theme load.
    `(fringe ((,c :background ,bg-main :foreground ,modus-nibelung-level-3)))
    `(margin ((,c :background ,bg-main)))
    `(line-number ((,c :background ,bg-main :foreground ,modus-nibelung-level-3)))
    `(line-number-current-line
      ((,c :background ,bg-main :foreground ,modus-nibelung-accent :weight semi-bold)))
    `(header-line ((,c :background ,modus-nibelung-level-0 :foreground ,fg-main :box nil)))
    `(header-line-inactive
      ((,c :background ,bg-main :foreground ,modus-nibelung-level-3 :box nil)))
    `(mode-line
      ((,c :background ,modus-nibelung-level-1 :foreground ,fg-main :box nil)))
    `(mode-line-active
      ((,c :background ,modus-nibelung-level-1 :foreground ,fg-main :box nil)))
    `(mode-line-inactive
      ((,c :background ,modus-nibelung-level-0 :foreground ,modus-nibelung-level-3 :box nil)))
    `(vertical-border ((,c :background ,bg-main :foreground ,modus-nibelung-level-0)))
    `(window-divider ((,c :background ,modus-nibelung-level-0 :foreground ,modus-nibelung-level-0)))
    `(window-divider-first-pixel
      ((,c :background ,modus-nibelung-level-0 :foreground ,modus-nibelung-level-0)))
    `(window-divider-last-pixel
      ((,c :background ,modus-nibelung-level-0 :foreground ,modus-nibelung-level-0)))

    ;; Markdown and markdown-ts-mode share one visual vocabulary.
    `(markdown-header-face ((,c :foreground ,modus-nibelung-level-6 :weight normal)))
    `(markdown-header-face-1 ((,c :inherit markdown-header-face :foreground ,modus-nibelung-level-6)))
    `(markdown-header-face-2 ((,c :inherit markdown-header-face :foreground ,modus-nibelung-level-5)))
    `(markdown-header-face-3 ((,c :inherit markdown-header-face :foreground ,modus-nibelung-level-4)))
    `(markdown-header-face-4 ((,c :inherit markdown-header-face :foreground ,blue)))
    `(markdown-header-face-5 ((,c :inherit markdown-header-face :foreground ,modus-nibelung-level-3)))
    `(markdown-header-face-6 ((,c :inherit markdown-header-face :foreground ,modus-nibelung-level-2)))
    `(markdown-bold-face ((,c :inherit bold)))
    `(markdown-italic-face ((,c :inherit italic)))
    `(markdown-code-face ((,c :inherit fixed-pitch :background ,modus-nibelung-level-0)))
    `(markdown-inline-code-face ((,c :inherit markdown-code-face)))
    `(markdown-pre-face ((,c :inherit markdown-code-face :extend t)))
    `(markdown-blockquote-face ((,c :foreground ,modus-nibelung-level-3 :slant italic)))
    `(markdown-markup-face ((,c :foreground ,modus-nibelung-level-3)))
    `(markdown-list-face ((,c :foreground ,modus-nibelung-level-4)))
    `(markdown-link-face ((,c :foreground ,modus-nibelung-accent :underline t)))
    `(markdown-table-face ((,c :inherit fixed-pitch)))
    `(markdown-ts-bold ((,c :inherit markdown-bold-face)))
    `(markdown-ts-emphasis ((,c :inherit markdown-italic-face)))
    `(markdown-ts-heading-1 ((,c :inherit markdown-header-face-1)))
    `(markdown-ts-heading-2 ((,c :inherit markdown-header-face-2)))
    `(markdown-ts-heading-3 ((,c :inherit markdown-header-face-3)))
    `(markdown-ts-heading-4 ((,c :inherit markdown-header-face-4)))
    `(markdown-ts-heading-5 ((,c :inherit markdown-header-face-5)))
    `(markdown-ts-heading-6 ((,c :inherit markdown-header-face-6)))
    `(markdown-ts-code-span ((,c :inherit markdown-inline-code-face)))
    `(markdown-ts-code-block ((,c :inherit markdown-pre-face :extend t)))
    `(markdown-ts-indented-code-block ((,c :inherit markdown-ts-code-block)))
    `(markdown-ts-code-block-markup-hidden
      ((,c :background ,modus-nibelung-level-0 :extend t)))
    `(markdown-ts-block-quote ((,c :inherit markdown-blockquote-face)))
    `(markdown-ts-delimiter ((,c :inherit markdown-markup-face)))
    `(markdown-ts-list-marker ((,c :inherit markdown-list-face)))
    `(markdown-ts-link ((,c :inherit markdown-link-face)))
    `(markdown-ts-link-destination ((,c :foreground ,modus-nibelung-accent :underline t)))
    `(markdown-ts-table ((,c :inherit markdown-table-face :extend t)))
    `(markdown-ts-table-header
      ((,c :inherit markdown-ts-table :foreground ,modus-nibelung-level-5 :weight semi-bold)))
    `(markdown-ts-table-delimiter-cell ((,c :foreground ,modus-nibelung-level-2)))
    `(markdown-ts-task-checked ((,c :foreground ,green :weight semi-bold)))
    `(markdown-ts-task-unchecked ((,c :foreground ,modus-nibelung-level-4)))

    ;; VUI primitives used by Mentat and other declarative interfaces.
    `(vui-field-placeholder ((,c :foreground ,modus-nibelung-level-3 :slant italic)))
    `(vui-table-header
      ((,c :background ,modus-nibelung-level-0 :foreground ,modus-nibelung-level-5 :weight semi-bold)))
    `(vui-table-border ((,c :foreground ,modus-nibelung-level-2)))

    ;; Mentat transcript surfaces and semantic states.
    `(mentat-muted-face ((,c :foreground ,modus-nibelung-level-3)))
    `(mentat-user-message-face
      ((,c :background ,modus-nibelung-accent-subtle :foreground ,fg-main :extend t)))
    `(mentat-user-marker-face ((,c :foreground ,modus-nibelung-level-4 :weight bold)))
    `(mentat-assistant-marker-face ((,c :foreground ,green :weight bold)))
    `(mentat-control-face
      ((,c :inherit fixed-pitch :foreground ,modus-nibelung-level-4)))
    `(mentat-queue-face ((,c :foreground ,modus-nibelung-level-3 :slant italic)))
    `(mentat-thinking-summary-face
      ((,c :foreground ,modus-nibelung-level-4 :weight semi-bold)))
    `(mentat-thinking-content-face ((,c :foreground ,modus-nibelung-level-3)))
    `(mentat-thinking-aborted-face ((,c :foreground ,red :weight semi-bold)))
    `(mentat-extension-progress-face ((,c :foreground ,yellow :weight semi-bold)))
    `(mentat-custom-message-label-face
      ((,c :foreground ,modus-nibelung-level-4 :weight bold)))
    `(mentat-custom-message-face ((,c :foreground ,modus-nibelung-level-3)))
    `(mentat-behavior-reminder-face
      ((,c :foreground ,modus-nibelung-level-3 :slant italic)))
    `(mentat-tool-running-face ((,c :foreground ,yellow :weight semi-bold)))
    `(mentat-tool-success-face ((,c :foreground ,green :weight semi-bold)))
    `(mentat-tool-error-face ((,c :foreground ,red :weight semi-bold)))
    `(mentat-tool-cancelled-face ((,c :foreground ,modus-nibelung-level-4 :weight semi-bold)))
    `(mentat-tool-name-face ((,c :foreground ,modus-nibelung-level-5 :weight bold)))
    `(mentat-tool-argument-face ((,c :foreground ,modus-nibelung-level-3)))
    `(mentat-tool-output-face ((,c :foreground ,modus-nibelung-level-4)))
    `(mentat-tool-line-number-face ((,c :foreground ,modus-nibelung-level-3)))
    `(mentat-tool-disclosure-face
      ((,c :foreground ,modus-nibelung-level-3 :slant italic :height 0.9)))
    `(mentat-run-prompt-face ((,c :foreground ,modus-nibelung-accent :weight semi-bold)))
    `(mentat-run-output-face
      ((,c :background ,modus-nibelung-level-0 :foreground ,fg-main :extend t)))
    `(mentat-run-output-border-face
      ((,c :background ,modus-nibelung-level-1 :foreground ,modus-nibelung-level-4
           :weight semi-bold :extend t)))
    `(mentat-code-block-face ((,c :background ,modus-nibelung-level-0 :extend t)))
    `(mentat-code-block-border-face
      ((,c :foreground ,modus-nibelung-level-3 :weight semi-bold)))
    `(mentat--code-block-opening-face
      ((,c :background ,modus-nibelung-level-1 :foreground ,modus-nibelung-level-4
           :weight semi-bold :extend t)))
    `(mentat-table-border-face ((,c :foreground ,modus-nibelung-level-2)))
    `(mentat-table-header-face
      ((,c :background ,modus-nibelung-level-1 :foreground ,modus-nibelung-level-5 :extend t)))
    `(mentat-table-alternate-row-face
      ((,c :background ,modus-nibelung-level-0 :extend t)))
    `(mentat-compaction-face ((,c :foreground ,magenta :weight semi-bold)))
    `(mentat-compaction-error-face ((,c :foreground ,red :weight semi-bold)))
    `(mentat-notification-info-face
      ((,c :foreground ,modus-nibelung-level-4 :weight semi-bold)))
    `(mentat-notification-warning-face ((,c :foreground ,yellow :weight semi-bold)))
    `(mentat-notification-error-face ((,c :foreground ,red :weight semi-bold)))
    `(mentat-mode-line-idle-face ((,c :foreground ,green :weight bold)))
    `(mentat-mode-line-active-face ((,c :foreground ,yellow :weight bold)))
    `(mentat-mode-line-error-face ((,c :foreground ,red :weight bold)))
    `(mentat-mode-line-metadata-face ((,c :foreground ,modus-nibelung-level-3)))
    `(mentat-mode-line-model-face ((,c :foreground ,modus-nibelung-level-6 :weight semi-bold)))
    `(mentat-mode-line-profile-face ((,c :foreground ,cyan :weight bold)))
    `(mentat-mode-line-context-warning-face ((,c :foreground ,yellow)))
    `(mentat-mode-line-context-error-face ((,c :foreground ,red)))
    `(mentat-reference-face ((,c :foreground ,modus-nibelung-accent :weight semi-bold)))
    `(mentat-image-caption-face
      ((,c :foreground ,modus-nibelung-level-3 :weight semi-bold)))
    `(mentat-open-working-face ((,c :foreground ,yellow :weight bold)))
    `(mentat-open-ready-face ((,c :foreground ,green)))
    `(mentat-open-attention-face ((,c :foreground ,yellow :weight bold)))
    `(mentat-open-closed-face ((,c :foreground ,modus-nibelung-level-3)))
    `(mentat-skill-face ((,c :foreground ,magenta :weight semi-bold)))
    `(mentat-subagent-name-face ((,c :foreground ,cyan :weight semi-bold)))
    `(mentat-subagent-foreground-face ((,c :foreground ,blue :weight semi-bold)))
    `(mentat-subagent-background-face ((,c :foreground ,magenta :weight semi-bold)))
    `(mentat-subagent-queued-face
      ((,c :foreground ,modus-nibelung-level-3 :weight semi-bold)))
    `(mentat-history-current-face
      ((,c :foreground ,modus-nibelung-accent :weight bold)))
    `(mentat-rpc-log-incoming-face ((,c :foreground ,green :weight bold)))
    `(mentat-rpc-log-outgoing-face ((,c :foreground ,blue :weight bold)))
    `(mentat-show-me-title-face
      ((,c :foreground ,modus-nibelung-level-5 :weight bold :height 1.2)))
    `(mentat-show-me-note-info-face ((,c :foreground ,modus-nibelung-level-4)))
    `(mentat-show-me-note-success-face ((,c :foreground ,green)))
    `(mentat-show-me-note-warning-face ((,c :foreground ,yellow))))
  "Faces shared by the light and dark modus-nibelung Modus themes.")

(provide 'modus-nibelung)
;;; modus-nibelung.el ends here
