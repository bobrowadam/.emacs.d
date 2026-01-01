;;; -*- lexical-binding: t; -*-

(defun eglot-hover-eldoc-function (cb &rest _ignored)
  (when (eglot-server-capable :hoverProvider)
    (let ((buf (current-buffer)))
      (eglot--async-request
       (eglot--current-server-or-lose)
       :textDocument/hover (eglot--TextDocumentPositionParams)
       :success-fn
       (eglot--lambda ((Hover) contents range)
         (eglot--when-buffer-window buf
           (let* ((info (unless (seq-empty-p contents)
                          (eglot--hover-info contents range)))
                  (echo
                                        ; ∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨∨
                   (if (and (eq major-mode 'rustic-mode) (stringp info))
                       (eglot-hover--hl-string
                        (eglot-hover--get "rust" (substring-no-properties info))
                        major-mode)
                                        ; ∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧∧
                     (let ((pos (and info (string-match "\n" info))))
                       (while (and pos (get-text-property pos 'invisible info))
                         (setq pos (string-match "\n" info (1+ pos))))
                       pos))))
             (funcall cb info :echo echo))))
       :hint :textDocument/hover))
    t))

(defun eglot-hover--hl-string (str mode)
  "Syntax highlight STR according to MODE."
  (with-temp-buffer
    (insert str)
    (delay-mode-hooks (funcall mode))
    (-each #'funcall
      (--remove (-contains? '(nil
                              rustic-setup-lsp
                              eglot--managed-mode
                              eldoc-mode
                              flymake-mode-off)
                            it)
                (--mapcat (ignore-errors (symbol-value it))
                          delayed-mode-hooks)))
    (font-lock-ensure)
    (buffer-string)))

(defun eglot-hover--get (lang str)
  "Get LANGs hover information in STR."
  (cl-flet ((join (sep strings)
              ;; This shields against Python shenanigans like
              ;;
              ;; def f(
              ;;   a,
              ;;   b
              ;; )
              (--reduce (concat acc
                                (if (or (s-suffix? "(" acc)
                                        (s-prefix? ")" it))
                                    it
                                  (concat sep it)))
                        strings)))
    (let* ((start (concat "```" lang))
           (groups (--filter (or (s-equals? start (car it))
                                 (s-equals? start (cadr it)))
                             (-partition-by #'s-blank?
                                            (s-lines (s-trim str)))))
           (name-at-point (symbol-name (symbol-at-point)))
           (type-sig-group (car
                            (--filter (--any? (s-contains? name-at-point it)
                                              it)
                                      groups))))
      (->> (or type-sig-group (car groups))
           (--drop-while (not (s-prefix? start it)))
           (-drop 1)                    ; ``` LANG
           (-drop-last 1)               ; ```
           (-map #'s-trim)
           (--filter (not (s-matches? comment-start-skip it)))
           (join " ")
           (s-chop-suffixes '("," "```" "``` ---"))))))
