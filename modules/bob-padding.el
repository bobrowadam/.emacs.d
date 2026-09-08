;;; bob-padding.el --- Fixed padding for Bob's graphical frames -*- lexical-binding: t; -*-

;;; Commentary:
;; Keep the spacing formerly supplied by Spacious Padding.  Face changes
;; are frame-local, so theme colours never become defaults for new frames.

;;; Code:

(require 'cl-lib)
(require 'tab-line)
(require 'cus-edit)

(defconst bob/padding-frame-parameters
  '((internal-border-width . 15)
    (right-divider-width . 30)
    (left-fringe . 8)
    (right-fringe . 8)
    (scroll-bar-width . 8))
  "Fixed frame spacing in pixels.")

(defconst bob/padding-boxes
  '((18 (mode-line default)
        (mode-line-active mode-line)
        (mode-line-inactive mode-line)
        (keycast-key default))
    (4 (header-line default)
       (header-line-inactive shadow)
       (tab-bar-tab tab-bar)
       (tab-bar-tab-inactive tab-bar)
       (tab-line-tab tab-line)
       (tab-line-tab-inactive tab-line)
       (tab-line-tab-current tab-line))
    (3 (custom-button default)
       (custom-button-mouse default)
       (custom-button-pressed default)))
  "Box widths followed by pairs of face and background fallback.")

(defconst bob/padding-blended-faces
  '(fringe margin line-number vertical-border
    window-divider window-divider-first-pixel window-divider-last-pixel)
  "Faces whose backgrounds blend into the frame background.")

(defun bob/padding-clear-saved-active-background ()
  "Remove the global active mode-line background left by the old advice."
  (when-let* ((defaults (cdr (gethash 'mode-line-active
                                      face--new-frame-defaults)))
              (background (face-attribute 'mode-line-active :background t))
              ((stringp background))
              (slot (cl-position background defaults :test #'equal)))
    (aset defaults slot 'unspecified)))

(defun bob/padding-retire-spacious-padding ()
  "Remove Spacious Padding state left in a running Emacs."
  (when (assq 'user (get 'spacious-padding-mode 'theme-value))
    (custom-theme-reset-variables 'user '(spacious-padding-mode nil)))
  (when (and (fboundp 'spacious-padding-mode)
             (bound-and-true-p spacious-padding-mode))
    (spacious-padding-mode -1))
  (when (and (fboundp 'spacious-padding-set-faces)
             (fboundp 'bob/fix-mode-line-active-bg))
    (advice-remove 'spacious-padding-set-faces
                   #'bob/fix-mode-line-active-bg))
  ;; Spacious Padding hides its synthetic theme from `custom-enabled-themes'.
  (when (custom-theme-p 'spacious-padding)
    (enable-theme 'spacious-padding)
    (disable-theme 'spacious-padding)))

(defun bob/padding-faces (frame)
  "Apply padding to FRAME without changing its underlying face specs."
  (when (display-graphic-p frame)
    ;; Remove our previous attributes before reading the current theme.
    ;; Unlike a box-only theme spec, this preserves defface inheritance.
    (dolist (face (append (mapcan (lambda (group) (mapcar #'car (cdr group)))
                                 bob/padding-boxes)
                         bob/padding-blended-faces
                         '(mode-line-highlight header-line-highlight)))
      (when (facep face)
        (face-spec-recalc face frame)))
    (dolist (group bob/padding-boxes)
      (dolist (entry (cdr group))
        (let ((face (car entry))
              (fallback (cadr entry)))
          (when (facep face)
            (let ((background (face-background face frame fallback)))
              (set-face-attribute face frame
                                  :box (list :line-width (car group)
                                             :color background :style nil))
              ;; Some themes explicitly omit both background and inheritance.
              (when (and (eq face 'mode-line-active)
                         (eq (face-attribute face :background frame t)
                             'unspecified))
                (set-face-attribute face frame :background background)))))))
    (let ((background (face-background 'default frame))
          (foreground (face-foreground 'default frame)))
      (dolist (face bob/padding-blended-faces)
        (set-face-attribute face frame :background background)
        (when (memq face '(vertical-border window-divider
                          window-divider-first-pixel window-divider-last-pixel))
          (set-face-attribute face frame :foreground background)))
      (dolist (face '(mode-line-highlight header-line-highlight))
        (set-face-attribute face frame :box (list :color foreground))))))

(defun bob/padding-refresh (&rest _)
  "Refresh padding colours on all frames after a theme change."
  (dolist (frame (frame-list))
    (bob/padding-faces frame)))

(defun bob/padding-frame (frame)
  "Set spacing and face padding on a new graphical FRAME."
  (when (display-graphic-p frame)
    (modify-frame-parameters frame bob/padding-frame-parameters)
    (bob/padding-faces frame)))

(defun bob/padding-enable ()
  "Install Bob's fixed padding at startup or after a divider-mode change."
  (bob/padding-retire-spacious-padding)
  (bob/padding-clear-saved-active-background)
  (dolist (parameter bob/padding-frame-parameters)
    (setf (alist-get (car parameter) default-frame-alist) (cdr parameter)))
  (dolist (frame (frame-list))
    (bob/padding-frame frame))
  (add-hook 'enable-theme-functions #'bob/padding-refresh)
  (add-hook 'disable-theme-functions #'bob/padding-refresh)
  (add-hook 'after-make-frame-functions #'bob/padding-frame)
  (add-hook 'window-divider-mode-hook #'bob/padding-enable))

(provide 'bob-padding)
;;; bob-padding.el ends here
