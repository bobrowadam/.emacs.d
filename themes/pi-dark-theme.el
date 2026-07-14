;;; pi-dark-theme.el --- Pi dark theme for Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Port of Pi's built-in `dark` palette.

;;; Code:

(deftheme pi-dark
  "Pi's built-in dark theme.")

(let* ((class '((class color) (min-colors 89)))
       ;; Pi dark palette: dist/modes/interactive/theme/dark.json.
       ;; Pi inherits Kitty's active terminal background.
       (bg "#1f1f28")
       (bg-alt "#1e1e24")
       (bg-pending "#282832")
       (bg-success "#283228")
       (bg-error "#3c2828")
       (bg-selected "#3a3a4a")
       (bg-custom "#2d2838")
       (fg "#dcd7ba")
       (gray "#808080")
       (dark-gray "#505050")
       (accent "#8abeb7")
       (cyan "#00d7ff")
       (blue "#5f87ff")
       (green "#b5bd68")
       (red "#cc6666")
       (yellow "#ffff00")
       (purple "#9575cd")
       (heading "#f0c674")
       (link "#81a2be")
       ;; Pi syntax palette.
       (comment "#6a9955")
       (keyword "#569cd6")
       (function "#dcdcaa")
       (variable "#9cdcfe")
       (string "#ce9178")
       (number "#b5cea8")
       (type "#4ec9b0"))
  (custom-theme-set-faces
   'pi-dark

   ;; Core UI
   `(default ((,class (:background ,bg :foreground ,fg))))
   `(cursor ((,class (:background ,accent))))
   `(fringe ((,class (:background ,bg :foreground ,dark-gray))))
   `(region ((,class (:background ,bg-selected))))
   `(highlight ((,class (:background ,bg-pending))))
   `(hl-line ((,class (:background ,bg-alt))))
   `(shadow ((,class (:foreground ,gray))))
   `(minibuffer-prompt ((,class (:foreground ,cyan :weight semi-bold))))
   `(vertical-border ((,class (:foreground ,dark-gray))))
   `(link ((,class (:foreground ,link :underline t))))
   `(link-visited ((,class (:foreground ,purple :underline t))))
   `(error ((,class (:foreground ,red :weight semi-bold))))
   `(warning ((,class (:foreground ,yellow :weight semi-bold))))
   `(success ((,class (:foreground ,green :weight semi-bold))))
   `(lazy-highlight ((,class (:background ,bg-selected :foreground ,fg))))
   `(match ((,class (:background ,accent :foreground ,bg :weight semi-bold))))
   `(trailing-whitespace ((,class (:background ,bg-error))))

   ;; Font lock
   `(font-lock-builtin-face ((,class (:foreground ,blue))))
   `(font-lock-comment-face ((,class (:foreground ,comment :slant italic))))
   `(font-lock-comment-delimiter-face ((,class (:foreground ,comment :slant italic))))
   `(font-lock-constant-face ((,class (:foreground ,number))))
   `(font-lock-doc-face ((,class (:foreground ,gray :slant italic))))
   `(font-lock-function-name-face ((,class (:foreground ,function :weight bold))))
   `(font-lock-keyword-face ((,class (:foreground ,keyword :weight bold))))
   `(font-lock-negation-char-face ((,class (:foreground ,red :weight bold))))
   `(font-lock-preprocessor-face ((,class (:foreground ,keyword))))
   `(font-lock-string-face ((,class (:foreground ,string))))
   `(font-lock-type-face ((,class (:foreground ,type :weight semi-bold))))
   `(font-lock-variable-name-face ((,class (:foreground ,variable))))
   `(font-lock-warning-face ((,class (:foreground ,red :weight bold))))

   ;; Window chrome and navigation
   `(mode-line ((,class (:background ,bg-pending :foreground ,fg
                                  :box (:line-width -1 :color ,bg-pending)))))
   `(mode-line-inactive ((,class (:background ,bg-alt :foreground ,gray
                                           :box (:line-width -1 :color ,bg-alt)))))
   `(mode-line-buffer-id ((,class (:foreground ,fg :weight bold))))
   `(header-line ((,class (:background ,bg-pending :foreground ,fg
                                    :box (:line-width -1 :color ,bg-pending)))))
   `(line-number ((,class (:background ,bg :foreground ,dark-gray))))
   `(line-number-current-line ((,class (:background ,bg :foreground ,accent :weight semi-bold))))
   `(isearch ((,class (:background ,yellow :foreground ,bg :weight bold))))
   `(isearch-fail ((,class (:background ,bg-error :foreground ,red :weight bold))))
   `(show-paren-match ((,class (:background ,bg-selected :foreground ,cyan :weight bold))))
   `(show-paren-mismatch ((,class (:background ,bg-error :foreground ,red :weight bold))))
   `(tooltip ((,class (:background ,bg-alt :foreground ,fg))))

   ;; Completion
   `(corfu-default ((,class (:background ,bg-alt :foreground ,fg))))
   `(corfu-current ((,class (:background ,bg-selected :foreground ,fg))))
   `(corfu-bar ((,class (:background ,accent))))
   `(corfu-border ((,class (:background ,dark-gray))))
   `(corfu-annotations ((,class (:foreground ,gray))))
   `(vertico-current ((,class (:background ,bg-selected :foreground ,fg))))
   `(vertico-group-title ((,class (:foreground ,heading :weight semi-bold))))
   `(vertico-group-separator ((,class (:foreground ,dark-gray :strike-through t))))
   `(orderless-match-face-0 ((,class (:foreground ,accent :weight bold :underline t))))
   `(orderless-match-face-1 ((,class (:foreground ,cyan :weight bold :underline t))))
   `(orderless-match-face-2 ((,class (:foreground ,heading :weight bold :underline t))))
   `(orderless-match-face-3 ((,class (:foreground ,purple :weight bold :underline t))))

   ;; Diffs and diagnostics
   `(diff-added ((,class (:background ,bg-success :foreground ,green))))
   `(diff-removed ((,class (:background ,bg-error :foreground ,red))))
   `(diff-changed ((,class (:background ,bg-pending :foreground ,blue))))
   `(diff-header ((,class (:background ,bg-alt :foreground ,fg))))
   `(diff-file-header ((,class (:background ,bg-selected :foreground ,fg :weight bold))))
   `(diff-refine-added ((,class (:background ,green :foreground ,bg))))
   `(diff-refine-removed ((,class (:background ,red :foreground ,bg))))
   `(diff-refine-changed ((,class (:background ,blue :foreground ,bg))))
   `(flymake-error ((,class (:underline (:style wave :color ,red)))))
   `(flymake-warning ((,class (:underline (:style wave :color ,yellow)))))
   `(flymake-note ((,class (:underline (:style wave :color ,blue)))))
   `(diff-hl-insert ((,class (:foreground ,green :background ,green))))
   `(diff-hl-delete ((,class (:foreground ,red :background ,red))))
   `(diff-hl-change ((,class (:foreground ,blue :background ,blue))))

   ;; Org and Markdown
   `(org-document-title ((,class (:foreground ,heading :weight bold :height 1.3))))
   `(org-level-1 ((,class (:foreground ,heading :weight bold :height 1.15))))
   `(org-level-2 ((,class (:foreground ,link :weight semi-bold :height 1.1))))
   `(org-level-3 ((,class (:foreground ,accent :weight semi-bold))))
   `(org-level-4 ((,class (:foreground ,purple))))
   `(org-drawer ((,class (:foreground ,gray))))
   `(org-special-keyword ((,class (:foreground ,gray))))
   `(org-property-value ((,class (:foreground ,gray))))
   `(org-code ((,class (:foreground ,accent :background ,bg-alt))))
   `(org-block ((,class (:background ,bg-alt :foreground ,green))))
   `(org-block-begin-line ((,class (:background ,bg-pending :foreground ,gray))))
   `(org-block-end-line ((,class (:background ,bg-pending :foreground ,gray))))
   `(org-quote ((,class (:background ,bg-alt :foreground ,gray :slant italic))))
   `(org-verbatim ((,class (:foreground ,accent))))
   `(org-link ((,class (:foreground ,link :underline t))))
   `(org-todo ((,class (:foreground ,red :weight bold))))
   `(org-done ((,class (:foreground ,green :weight bold))))
   `(markdown-header-face ((,class (:foreground ,heading :weight bold))))
   `(markdown-code-face ((,class (:foreground ,accent))))
   `(markdown-inline-code-face ((,class (:foreground ,accent))))

   ;; Mentat's chat presentation, mapped directly to Pi message/tool tokens.
   `(mentat-live-tool-running-face ((,class (:background ,bg-pending :extend t))))
   `(mentat-live-tool-done-face ((,class (:background ,bg-success :extend t))))
   `(mentat-live-tool-error-face ((,class (:background ,bg-error :extend t))))
   `(mentat-live-tool-cancelled-face ((,class (:background ,bg-pending :extend t))))
   `(mentat-live-tool-name-face ((,class (:foreground ,fg :weight bold))))
   `(mentat-live-tool-args-face ((,class (:foreground ,gray))))
   `(mentat-live-thinking-face ((,class (:foreground ,gray :slant italic :extend t))))
   `(mentat-compaction-face ((,class (:background ,bg-custom :foreground ,fg :extend t))))
   `(mentat-compaction-label-face ((,class (:background ,bg-custom :foreground ,purple
                                                    :weight bold :extend t))))
   `(mentat-control-message-face ((,class (:background ,bg-custom :foreground ,fg :extend t))))
   `(mentat-control-message-label-face ((,class (:background ,bg-custom :foreground ,purple
                                                         :weight bold :extend t))))
   `(mentat-skill-message-face ((,class (:background ,bg-custom :foreground ,fg :extend t))))
   `(mentat-skill-message-label-face ((,class (:background ,bg-custom :foreground ,purple
                                                       :weight bold :extend t))))

   ;; Magit
   `(magit-section-heading ((,class (:foreground ,heading :weight bold))))
   `(magit-branch-local ((,class (:foreground ,link))))
   `(magit-branch-remote ((,class (:foreground ,green))))
   `(magit-diff-added ((,class (:background ,bg-success :foreground ,green))))
   `(magit-diff-added-highlight ((,class (:background "#344234" :foreground ,green))))
   `(magit-diff-removed ((,class (:background ,bg-error :foreground ,red))))
   `(magit-diff-removed-highlight ((,class (:background "#4a3030" :foreground ,red))))
   `(magit-diff-context ((,class (:foreground ,gray))))
   `(magit-diff-context-highlight ((,class (:background ,bg-alt :foreground ,gray))))
   `(magit-diff-file-heading ((,class (:foreground ,fg :weight semi-bold))))
   `(magit-diff-file-heading-highlight ((,class (:background ,bg-pending :foreground ,fg :weight semi-bold))))
   `(magit-diff-hunk-heading ((,class (:background ,bg-pending :foreground ,gray))))
   `(magit-hash ((,class (:foreground ,gray))))

   ;; Files and terminals
   `(dired-directory ((,class (:foreground ,link :weight bold))))
   `(dired-header ((,class (:foreground ,heading :weight bold))))
   `(dired-symlink ((,class (:foreground ,purple))))
   `(dired-mark ((,class (:foreground ,accent :weight bold))))
   `(dired-flagged ((,class (:foreground ,red :weight bold))))
   `(dired-ignored ((,class (:foreground ,dark-gray))))
   `(term-color-black ((,class (:background ,bg :foreground ,bg))))
   `(term-color-red ((,class (:background ,red :foreground ,red))))
   `(term-color-green ((,class (:background ,green :foreground ,green))))
   `(term-color-yellow ((,class (:background ,yellow :foreground ,yellow))))
   `(term-color-blue ((,class (:background ,blue :foreground ,blue))))
   `(term-color-magenta ((,class (:background ,purple :foreground ,purple))))
   `(term-color-cyan ((,class (:background ,cyan :foreground ,cyan))))
   `(term-color-white ((,class (:background ,fg :foreground ,fg))))
   `(ansi-color-black ((,class (:foreground ,bg))))
   `(ansi-color-red ((,class (:foreground ,red))))
   `(ansi-color-green ((,class (:foreground ,green))))
   `(ansi-color-yellow ((,class (:foreground ,yellow))))
   `(ansi-color-blue ((,class (:foreground ,blue))))
   `(ansi-color-magenta ((,class (:foreground ,purple))))
   `(ansi-color-cyan ((,class (:foreground ,cyan))))
   `(ansi-color-white ((,class (:foreground ,fg)))))

  (custom-theme-set-variables
   'pi-dark
   `(ansi-color-names-vector [,bg ,red ,green ,yellow ,blue ,purple ,cyan ,fg])))

(defun pi-dark--refresh-mentat-faces (&rest _)
  "Restore Pi's exact tool-card colors after Mentat derives its faces."
  (when (memq 'pi-dark custom-enabled-themes)
    (dolist (face-spec '((mentat-live-tool-running-face "#282832")
                         (mentat-live-tool-done-face "#283228")
                         (mentat-live-tool-error-face "#3c2828")
                         (mentat-live-tool-cancelled-face "#282832")))
      (when (facep (car face-spec))
        (set-face-attribute (car face-spec) nil
                            :background (cadr face-spec)
                            :extend t)))))

(add-hook 'enable-theme-functions #'pi-dark--refresh-mentat-faces t)

(provide-theme 'pi-dark)

;;; pi-dark-theme.el ends here
