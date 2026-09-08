;;; bob-padding-tests.el --- Padding regression tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'bob-padding)

(deftheme bob-padding-test-inherited)
(custom-theme-set-faces
 'bob-padding-test-inherited
 '(default ((t (:background "#111111" :foreground "#dddddd"))))
 '(mode-line ((t (:background "#334455" :foreground "#eeeeee"))))
 '(header-line ((t (:background "#223344" :foreground "#eeeeee")))))

(deftheme bob-padding-test-explicit)
(custom-theme-set-faces
 'bob-padding-test-explicit
 '(default ((t (:background "#eeeeee" :foreground "#111111"))))
 '(mode-line ((t (:background "#cccccc" :foreground "#222222"))))
 '(mode-line-active ((t (:background "#aabbcc" :foreground "#112233"))))
 '(header-line ((t (:background "#dddddd" :foreground "#222222")))))

(deftheme bob-padding-test-missing)
(custom-theme-set-faces
 'bob-padding-test-missing
 '(mode-line ((t (:background "#334455" :foreground "#eeeeee"))))
 '(mode-line-active ((t (:foreground "#abcdef")))))

(defmacro bob/padding-test-environment (&rest body)
  "Run BODY with graphical padding enabled on the batch frame."
  (declare (indent 0) (debug t))
  `(let ((inhibit-frame-set-background-mode t)
         (enable-theme-functions nil)
         (disable-theme-functions nil)
         (after-make-frame-functions nil)
         (window-divider-mode-hook nil)
         (default-frame-alist (copy-tree default-frame-alist)))
     (cl-letf (((symbol-function 'display-graphic-p) (lambda (&optional _) t)))
       (unwind-protect
           (progn (bob/padding-enable) ,@body)
         (mapc #'disable-theme (copy-sequence custom-enabled-themes))))))

(defun bob/padding-test-active (background)
  "Check active background and box colour against BACKGROUND."
  (should (equal (face-background 'mode-line-active nil t) background))
  (should (equal (face-foreground 'mode-line-active nil t)
                 (if (equal background "#aabbcc") "#112233" "#eeeeee")))
  (should (equal (face-attribute 'mode-line-active :box)
                 (list :line-width 18 :color background :style nil))))

(ert-deftest bob/padding-theme-switch-and-frame-initialization ()
  (bob/padding-test-environment
    (dolist (theme '(bob-padding-test-inherited bob-padding-test-explicit
                    bob-padding-test-inherited))
      (mapc #'disable-theme (copy-sequence custom-enabled-themes))
      (enable-theme theme)
      (let ((background (if (eq theme 'bob-padding-test-explicit)
                            "#aabbcc" "#334455")))
        (bob/padding-test-active background)
        (face-set-after-frame-default (selected-frame))
        (run-hook-with-args 'after-make-frame-functions (selected-frame))
        (bob/padding-test-active background)))))

(ert-deftest bob/padding-preserves-inheritance-and-explicit-foreground ()
  (bob/padding-test-environment
    (enable-theme 'bob-padding-test-inherited)
    (should (eq (face-attribute 'mode-line-active :inherit) 'mode-line))
    (enable-theme 'bob-padding-test-missing)
    (should (equal (face-background 'mode-line-active nil t) "#334455"))
    (should (equal (face-foreground 'mode-line-active nil t) "#abcdef"))))

(ert-deftest bob/padding-does-not-store-global-face-attributes ()
  (let ((before (copy-tree (gethash 'mode-line-active face--new-frame-defaults) t)))
    (bob/padding-test-environment
      (enable-theme 'bob-padding-test-missing)
      (bob/padding-refresh)
      (should (equal before (gethash 'mode-line-active face--new-frame-defaults))))))

(ert-deftest bob/padding-clears-old-saved-background ()
  (let ((before (copy-tree (gethash 'mode-line-active
                                    face--new-frame-defaults) t)))
    (unwind-protect
        (progn
          (set-face-attribute 'mode-line-active t :background "#abcdef")
          (bob/padding-clear-saved-active-background)
          (should (eq (face-attribute 'mode-line-active :background t)
                      'unspecified)))
      (puthash 'mode-line-active before face--new-frame-defaults))))

(ert-deftest bob/padding-retires-old-custom-value ()
  (let ((theme-value (get 'spacious-padding-mode 'theme-value))
        reset)
    (unwind-protect
        (progn
          (put 'spacious-padding-mode 'theme-value '((user t)))
          (cl-letf (((symbol-function 'custom-theme-reset-variables)
                     (lambda (&rest args) (setq reset args))))
            (bob/padding-retire-spacious-padding))
          (should (equal reset '(user (spacious-padding-mode nil)))))
      (put 'spacious-padding-mode 'theme-value theme-value))))

(ert-deftest bob/padding-keeps-current-dimensions-and-is-idempotent ()
  (bob/padding-test-environment
    (enable-theme 'bob-padding-test-inherited)
    (dolist (parameter '((internal-border-width . 15) (right-divider-width . 30)
                         (left-fringe . 8) (right-fringe . 8) (scroll-bar-width . 8)))
      (should (equal (frame-parameter nil (car parameter)) (cdr parameter))))
    (dolist (pair '((mode-line . 18) (mode-line-active . 18)
                    (mode-line-inactive . 18) (header-line . 4)
                    (header-line-inactive . 4) (tab-bar-tab . 4)
                    (tab-line-tab-current . 4) (custom-button . 3)))
      (should (equal (plist-get (face-attribute (car pair) :box) :line-width)
                     (cdr pair))))
    (let ((before (face-all-attributes 'mode-line-active)))
      (bob/padding-refresh)
      (should (equal before (face-all-attributes 'mode-line-active))))
    (dolist (face '(fringe margin line-number vertical-border window-divider
                   window-divider-first-pixel window-divider-last-pixel))
      (should (equal (face-background face) "#111111")))))

(provide 'bob-padding-tests)
;;; bob-padding-tests.el ends here
