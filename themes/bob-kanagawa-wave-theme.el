;;; bob-kanagawa-wave-theme.el --- Great Wave inspired DARK theme -*- lexical-binding: t; -*-

;; Author: OpenAI
;; Version: 0.3.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: faces, theme
;; URL: https://example.com/bob-kanagawa-wave

;;; Commentary:
;;
;; A dark theme drawn from the actual palette of Hokusai's Great Wave:
;; deep indigo ink, muted steel-blue water, pale foam, warm tan sky,
;; dull olive-gold boats, and earthy warm grays.
;;
;; Usage:
;;   1. Put this file somewhere in your `custom-theme-load-path`
;;   2. M-x load-theme RET bob-kanagawa-wave RET
;;

;;; Code:

(deftheme bob-kanagawa-wave
  "Great Wave inspired DARK theme — muted, earthy, ukiyo-e tones.")

(let* ((class '((class color) (min-colors 89)))

       ;; Backgrounds — deep ocean
       (bg          "#1A1E2E") ; dark indigo-gray
       (bg-alt      "#222738") ; slightly lighter
       (bg-dim      "#2C3246") ; selections, active regions
       (bg-active   "#252A3C")
       (bg-highlight "#2E3448")

       ;; Foregrounds — foam and mist
       (fg          "#C8D5E0") ; pale foam — main text
       (fg-alt      "#A8B5C2") ; muted blue-gray
       (fg-muted    "#6E7A87") ; warm gray — comments, line numbers
       (fg-dim      "#505A66") ; very subdued

       ;; Structural
       (border      "#3A4255")
       (cursor      "#A09850") ; boat gold

       ;; The actual Hokusai palette
       (indigo      "#2B3A67") ; deep ink outlines
       (indigo-light "#4A6490") ; lighter ink
       (steel       "#6B88A8") ; wave body blue
       (steel-light "#8BA4BC") ; lighter steel
       (foam        "#C8D5E0") ; white foam
       (foam-bright "#DDE6ED") ; bright foam
       (tan         "#B8A88A") ; sky/sand
       (tan-dim     "#8E8878") ; warm mist gray
       (tan-light   "#D0C4A8") ; light sand
       (gold        "#A09850") ; boat olive-gold
       (gold-bright "#BDB460") ; brighter gold
       (rust        "#A06050") ; muted warm red (from seal stamp)
       (rust-light  "#C07868") ; lighter rust
       (clay        "#907060") ; earthy brown-red
       (sage        "#708068") ; muted green (sea foam tint)
       (sage-light  "#90A088") ; lighter sage
       (plum        "#7A6882") ; muted purple (twilight sky)
       (plum-light  "#9A88A2") ; lighter plum

       ;; Functional
       (diff-add    "#283828")
       (diff-del    "#382828")
       (diff-change "#282E38")
       (warning-bg  "#38332A")
       (error-bg    "#382A2A")
       (success-bg  "#2A3828"))

  (custom-theme-set-faces
   'bob-kanagawa-wave

   ;; Basic UI
   `(default ((,class (:background ,bg :foreground ,fg))))
   `(cursor ((,class (:background ,cursor))))
   `(fringe ((,class (:background ,bg :foreground ,fg-dim))))
   `(region ((,class (:background ,bg-dim))))
   `(highlight ((,class (:background ,bg-highlight))))
   `(hl-line ((,class (:background ,bg-alt))))
   `(shadow ((,class (:foreground ,fg-muted))))
   `(minibuffer-prompt ((,class (:foreground ,steel-light :weight semi-bold))))
   `(vertical-border ((,class (:foreground ,border))))
   `(link ((,class (:foreground ,steel-light :underline t))))
   `(link-visited ((,class (:foreground ,plum-light :underline t))))
   `(error ((,class (:foreground ,rust-light :weight semi-bold))))
   `(warning ((,class (:foreground ,gold-bright :weight semi-bold))))
   `(success ((,class (:foreground ,sage-light :weight semi-bold))))
   `(lazy-highlight ((,class (:background ,gold :foreground ,bg))))
   `(match ((,class (:background ,gold :foreground ,bg :weight semi-bold))))
   `(trailing-whitespace ((,class (:background ,error-bg))))
   `(escape-glyph ((,class (:foreground ,plum-light))))
   `(homoglyph ((,class (:foreground ,plum-light))))

   ;; Font lock — the heart of it
   `(font-lock-builtin-face ((,class (:foreground ,steel-light))))
   `(font-lock-comment-face ((,class (:foreground "#7D8793" :slant italic))))
   `(font-lock-comment-delimiter-face ((,class (:foreground "#67707C" :slant italic))))
   `(font-lock-constant-face ((,class (:foreground ,gold-bright :weight semi-bold))))
   `(font-lock-doc-face ((,class (:foreground "#8A939D" :slant italic))))
   `(font-lock-function-name-face ((,class (:foreground "#CEC470" :weight bold))))
   `(font-lock-keyword-face ((,class (:foreground "#9BB4CC" :weight bold))))
   `(font-lock-negation-char-face ((,class (:foreground ,rust :weight bold))))
   `(font-lock-preprocessor-face ((,class (:foreground ,clay))))
   `(font-lock-regexp-grouping-backslash ((,class (:foreground ,clay :weight bold))))
   `(font-lock-regexp-grouping-construct ((,class (:foreground ,plum :weight bold))))
   `(font-lock-string-face ((,class (:foreground ,tan))))
   `(font-lock-type-face ((,class (:foreground ,foam-bright :weight semi-bold))))
   `(eglot-semantic-declaration ((,class (:foreground ,foam-bright :weight bold))))
   `(eglot-semantic-type ((,class (:foreground ,foam-bright :weight semi-bold))))
   `(font-lock-variable-name-face ((,class (:foreground ,foam))))
   `(font-lock-warning-face ((,class (:foreground ,rust-light :weight bold))))

   ;; Mode line / header line
   `(mode-line ((,class (:background ,bg-dim :foreground ,fg :box (:line-width -1 :color ,bg-dim)))))
   `(mode-line-inactive ((,class (:background ,bg-alt :foreground ,fg-muted :box (:line-width -1 :color ,bg-alt)))))
   `(mode-line-buffer-id ((,class (:foreground ,foam-bright :weight bold))))
   `(header-line ((,class (:background ,bg-active :foreground ,fg :box (:line-width -1 :color ,bg-active)))))

   ;; Line numbers
   `(line-number ((,class (:background ,bg :foreground ,fg-dim))))
   `(line-number-current-line ((,class (:background ,bg :foreground ,gold :weight semi-bold))))

   ;; Search / isearch
   `(isearch ((,class (:background ,gold :foreground ,bg :weight bold))))
   `(isearch-fail ((,class (:background ,error-bg :foreground ,rust-light :weight bold))))
   `(query-replace ((,class (:inherit isearch))))

   ;; Parens
   `(show-paren-match ((,class (:background ,bg-dim :foreground ,foam-bright :weight bold))))
   `(show-paren-mismatch ((,class (:background ,error-bg :foreground ,rust-light :weight bold))))

   ;; Completion / popup-like areas
   `(tooltip ((,class (:background ,bg-alt :foreground ,fg))))
   `(completions-common-part ((,class (:foreground ,gold-bright :weight bold :underline t))))
   `(completions-first-difference ((,class (:foreground ,rust-light :weight semi-bold))))

   ;; Diff / VC
   `(diff-added ((,class (:background ,diff-add :foreground ,sage-light))))
   `(diff-removed ((,class (:background ,diff-del :foreground ,rust-light))))
   `(diff-changed ((,class (:background ,diff-change :foreground ,steel-light))))
   `(diff-header ((,class (:background ,bg-active :foreground ,fg))))
   `(diff-file-header ((,class (:background ,bg-dim :foreground ,foam-bright :weight bold))))
   `(diff-refine-added ((,class (:background ,success-bg :foreground ,sage-light :weight semi-bold))))
   `(diff-refine-removed ((,class (:background ,error-bg :foreground ,rust-light :weight semi-bold))))
   `(diff-refine-changed ((,class (:background ,bg-dim :foreground ,steel-light :weight semi-bold))))
   `(vc-edited-state ((,class (:foreground ,gold))))
   `(vc-conflict-state ((,class (:foreground ,rust-light))))
   `(vc-locally-added-state ((,class (:foreground ,sage))))
   `(vc-up-to-date-state ((,class (:foreground ,steel))))

   ;; Org
   `(org-document-title ((,class (:foreground ,foam-bright :weight bold :height 1.3))))
   `(org-document-info ((,class (:foreground ,fg-alt))))
   `(org-level-1 ((,class (:foreground ,tan-light :weight bold :height 1.15))))
   `(org-level-2 ((,class (:foreground ,steel-light :weight semi-bold :height 1.1))))
   `(org-level-3 ((,class (:foreground ,gold-bright :weight semi-bold))))
   `(org-level-4 ((,class (:foreground ,plum-light))))
   `(org-code ((,class (:foreground ,tan-light :background ,bg-alt))))
   `(org-block ((,class (:background ,bg-alt :foreground ,fg))))
   `(org-block-begin-line ((,class (:background ,bg-active :foreground ,fg-muted))))
   `(org-block-end-line ((,class (:background ,bg-active :foreground ,fg-muted))))
   `(org-quote ((,class (:background ,bg-alt :slant italic))))
   `(org-verbatim ((,class (:foreground ,steel-light))))
   `(org-link ((,class (:foreground ,steel-light :underline t))))
   `(org-date ((,class (:foreground ,gold-bright :underline t))))
   `(org-warning ((,class (:foreground ,rust-light :weight bold))))
   `(org-todo ((,class (:foreground ,rust-light :weight bold))))
   `(org-done ((,class (:foreground ,sage :weight bold))))
   `(org-headline-done ((,class (:foreground ,fg-muted))))

   ;; Magit
   `(magit-section-heading ((,class (:foreground ,gold-bright :weight bold))))
   `(magit-branch-local ((,class (:foreground ,steel-light))))
   `(magit-branch-remote ((,class (:foreground ,sage-light))))
   `(magit-diff-added ((,class (:background ,diff-add :foreground ,sage-light))))
   `(magit-diff-added-highlight ((,class (:background "#2A4232" :foreground ,sage-light))))
   `(magit-diff-removed ((,class (:background ,diff-del :foreground ,rust-light))))
   `(magit-diff-removed-highlight ((,class (:background "#422A2A" :foreground ,rust-light))))
   `(magit-diff-context ((,class (:foreground ,fg-muted))))
   `(magit-diff-context-highlight ((,class (:background ,bg-alt :foreground ,fg-muted))))
   `(magit-diff-file-heading ((,class (:foreground ,foam :weight semi-bold))))
   `(magit-diff-file-heading-highlight ((,class (:background ,bg-active :foreground ,foam-bright :weight semi-bold))))
   `(magit-diff-hunk-heading ((,class (:background ,bg-active :foreground ,fg-alt))))
   `(magit-diff-hunk-heading-highlight ((,class (:background ,bg-dim :foreground ,foam))))
   `(magit-diff-lines-heading ((,class (:background ,bg-dim :foreground ,gold-bright))))
   `(magit-hash ((,class (:foreground ,fg-muted))))

   ;; Company / Corfu / generic completions
   `(company-tooltip ((,class (:background ,bg-alt :foreground ,fg))))
   `(company-tooltip-selection ((,class (:background ,bg-dim :foreground ,fg))))
   `(company-tooltip-common ((,class (:foreground ,gold-bright :weight semi-bold))))
   `(company-tooltip-annotation ((,class (:foreground ,tan-dim))))
   `(company-scrollbar-bg ((,class (:background ,bg-active))))
   `(company-scrollbar-fg ((,class (:background ,fg-dim))))
   `(company-preview ((,class (:background ,bg-alt :foreground ,fg-muted))))
   `(company-preview-common ((,class (:foreground ,gold-bright :weight semi-bold))))

   ;; Corfu
   `(corfu-default ((,class (:background ,bg-active))))
   `(corfu-current ((,class (:background ,bg-dim :foreground ,foam-bright :weight semi-bold))))
   `(corfu-bar ((,class (:background ,fg-dim))))
   `(corfu-border ((,class (:background ,border))))
   `(corfu-annotations ((,class (:foreground ,fg-muted))))
   `(corfu-deprecated ((,class (:foreground ,fg-dim :strike-through t))))

   ;; ido / ivy / helm-ish common faces
   `(ivy-current-match ((,class (:background ,bg-dim :foreground ,foam-bright :weight semi-bold))))
   `(ivy-minibuffer-match-face-1 ((,class (:foreground ,fg))))
   `(ivy-minibuffer-match-face-2 ((,class (:foreground ,gold-bright :weight semi-bold))))
   `(ivy-minibuffer-match-face-3 ((,class (:foreground ,steel-light :weight semi-bold))))
   `(ivy-minibuffer-match-face-4 ((,class (:foreground ,plum-light :weight semi-bold))))

   ;; Terminal colors
   `(term-color-black ((,class (:background ,bg :foreground ,bg))))
   `(term-color-blue ((,class (:background ,steel :foreground ,steel))))
   `(term-color-cyan ((,class (:background ,steel-light :foreground ,steel-light))))
   `(term-color-green ((,class (:background ,sage :foreground ,sage))))
   `(term-color-magenta ((,class (:background ,plum :foreground ,plum))))
   `(term-color-red ((,class (:background ,rust :foreground ,rust))))
   `(term-color-white ((,class (:background ,foam :foreground ,foam))))
   `(term-color-yellow ((,class (:background ,gold :foreground ,gold))))

   ;; Rainbow delimiters
   `(rainbow-delimiters-depth-1-face ((,class (:foreground ,steel-light))))
   `(rainbow-delimiters-depth-2-face ((,class (:foreground ,gold-bright))))
   `(rainbow-delimiters-depth-3-face ((,class (:foreground ,plum-light))))
   `(rainbow-delimiters-depth-4-face ((,class (:foreground ,sage-light))))
   `(rainbow-delimiters-depth-5-face ((,class (:foreground ,tan-light))))
   `(rainbow-delimiters-depth-6-face ((,class (:foreground ,rust-light))))
   `(rainbow-delimiters-depth-7-face ((,class (:foreground ,clay))))
   `(rainbow-delimiters-unmatched-face ((,class (:foreground ,rust-light :weight bold))))

   ;; Whitespace-mode
   `(whitespace-space ((,class (:foreground ,fg-dim))))
   `(whitespace-tab ((,class (:foreground ,fg-dim))))
   `(whitespace-newline ((,class (:foreground ,fg-dim))))
   `(whitespace-trailing ((,class (:background ,error-bg))))
   `(whitespace-line ((,class (:background ,warning-bg :foreground ,gold))))

   ;; Vertico
   `(vertico-current ((,class (:background ,bg-dim))))
   `(vertico-group-title ((,class (:foreground ,tan-light :weight semi-bold :slant italic))))
   `(vertico-group-separator ((,class (:foreground ,border :strike-through t))))
   `(vertico-multiline ((,class (:foreground ,fg-muted))))

   ;; Orderless
   `(orderless-match-face-0 ((,class (:foreground ,gold-bright :weight bold :underline t))))
   `(orderless-match-face-1 ((,class (:foreground "#7CB8D4" :weight bold :underline t))))
   `(orderless-match-face-2 ((,class (:foreground ,rust-light :weight bold :underline t))))
   `(orderless-match-face-3 ((,class (:foreground ,plum-light :weight bold :underline t))))

   ;; Dired
   `(dired-directory ((,class (:foreground ,steel-light :weight bold))))
   `(dired-header ((,class (:foreground ,tan-light :weight bold))))
   `(dired-symlink ((,class (:foreground ,plum-light))))
   `(dired-broken-symlink ((,class (:foreground ,rust-light :strike-through t))))
   `(dired-mark ((,class (:foreground ,gold-bright :weight bold))))
   `(dired-marked ((,class (:foreground ,gold-bright))))
   `(dired-flagged ((,class (:foreground ,rust-light :weight bold))))
   `(dired-ignored ((,class (:foreground ,fg-dim))))
   `(dired-perm-write ((,class (:foreground ,sage))))
   `(dired-warning ((,class (:foreground ,rust-light :weight bold))))

   ;; Diredfl
   `(diredfl-dir-name ((,class (:foreground ,steel-light :weight bold))))
   `(diredfl-dir-heading ((,class (:foreground ,tan-light :weight bold))))
   `(diredfl-file-name ((,class (:foreground ,fg))))
   `(diredfl-file-suffix ((,class (:foreground ,fg-alt))))
   `(diredfl-ignored-file-name ((,class (:foreground ,fg-dim))))
   `(diredfl-compressed-file-name ((,class (:foreground ,plum-light))))
   `(diredfl-compressed-file-suffix ((,class (:foreground ,plum-light))))
   `(diredfl-symlink ((,class (:foreground ,plum-light))))
   `(diredfl-date-time ((,class (:foreground ,fg-muted))))
   `(diredfl-number ((,class (:foreground ,fg-muted))))
   `(diredfl-dir-priv ((,class (:foreground ,steel-light))))
   `(diredfl-read-priv ((,class (:foreground ,fg-muted))))
   `(diredfl-write-priv ((,class (:foreground ,sage))))
   `(diredfl-exec-priv ((,class (:foreground ,rust-light))))
   `(diredfl-no-priv ((,class (:foreground ,fg-dim))))
   `(diredfl-rare-priv ((,class (:foreground ,gold))))
   `(diredfl-other-priv ((,class (:foreground ,tan-dim))))
   `(diredfl-link-priv ((,class (:foreground ,plum-light))))
   `(diredfl-autofile-name ((,class (:foreground ,fg-alt))))
   `(diredfl-tagged-autofile-name ((,class (:foreground ,fg-alt))))
   `(diredfl-flag-mark ((,class (:foreground ,gold-bright :weight bold))))
   `(diredfl-flag-mark-line ((,class (:background ,bg-highlight))))
   `(diredfl-deletion ((,class (:foreground ,rust-light :weight bold))))
   `(diredfl-deletion-file-name ((,class (:foreground ,rust-light))))
   `(diredfl-executable-tag ((,class (:foreground ,sage-light))))

   ;; Flymake
   `(flymake-error ((,class (:background "#4A2A2A"))))
   `(flymake-warning ((,class (:underline (:style wave :color ,gold)))))
   `(flymake-note ((,class (:underline (:style wave :color ,steel)))))
   `(flymake-error-fringe ((,class (:foreground "#E05565"))))
   `(flymake-warning-fringe ((,class (:foreground ,gold))))
   `(flymake-note-fringe ((,class (:foreground ,steel))))

   ;; Jinx (spelling)
   `(jinx-misspelled ((,class (:underline (:style wave :color ,tan-dim)))))
   `(jinx-highlight ((,class (:background ,bg-dim))))
   `(jinx-key ((,class (:foreground ,gold-bright :weight bold))))

   ;; Diff-hl (git gutter)
   `(diff-hl-insert ((,class (:foreground ,sage :background ,sage))))
   `(diff-hl-delete ((,class (:foreground ,rust :background ,rust))))
   `(diff-hl-change ((,class (:foreground ,gold :background ,gold))))

   ;; ANSI colors for compilation buffers
   `(ansi-color-black ((,class (:foreground ,bg))))
   `(ansi-color-blue ((,class (:foreground ,steel))))
   `(ansi-color-cyan ((,class (:foreground ,steel-light))))
   `(ansi-color-green ((,class (:foreground ,sage))))
   `(ansi-color-magenta ((,class (:foreground ,plum))))
   `(ansi-color-red ((,class (:foreground ,rust))))
   `(ansi-color-white ((,class (:foreground ,foam))))
   `(ansi-color-yellow ((,class (:foreground ,gold)))))

  (custom-theme-set-variables
   'bob-kanagawa-wave
   `(ansi-color-names-vector [,bg ,rust ,sage ,gold ,steel ,plum ,steel-light ,foam])))

(provide-theme 'bob-kanagawa-wave)

;;; bob-kanagawa-wave-theme.el ends here
