;;; bob-gpu-snapshot-tests.el --- Snapshot layout checks -*- lexical-binding: t; -*-

(require 'ert)
(require 'bob-gpu-snapshot)

(ert-deftest bob/gpu-snapshot-context-excludes-runtime-graphs ()
  (with-temp-buffer
    (insert "Working\n")
    (let* ((runtime (list (make-string (* 1024 1024) ?x) (current-buffer)))
           (callback (lambda () runtime))
           (spacer (propertize "\n" 'line-height 2.0 'help-echo callback))
           (overlay (make-overlay (point-min) (point-max))))
      (add-text-properties (point-min) (point-max)
                           (list 'face 'bold 'action callback
                                 'help-echo callback 'mentat-state runtime))
      (overlay-put overlay 'before-string spacer)
      (overlay-put overlay 'priority 95)
      (overlay-put overlay 'action callback)
      (let* ((context (bob/gpu-snapshot--context (point-min) (point-max)))
             (properties (cdar (plist-get context :properties)))
             (overlay-properties (nth 2 (car (plist-get context :overlays))))
             (saved-spacer (plist-get overlay-properties 'before-string))
             (printed (let ((print-circle t)) (pp-to-string context))))
        (should (equal properties '(face bold)))
        (should (= (plist-get overlay-properties 'priority) 95))
        (should (= (get-text-property 0 'line-height saved-spacer) 2.0))
        (should-not (get-text-property 0 'help-echo saved-spacer))
        (should-not (plist-get overlay-properties 'action))
        (should (< (length printed) 1000))
        (should (equal (read printed) context))))))

(ert-deftest bob/gpu-snapshot-layout-bounds-self-referencing-strings ()
  (let ((text (copy-sequence "cycle")))
    (put-text-property 0 (length text) 'display text text)
    (let* ((properties (bob/gpu-snapshot--layout-properties (list 'display text)))
           (printed (let ((print-circle t)) (pp-to-string properties))))
      (should (< (length printed) 5000))
      (should (string-match-p ":truncated" printed))
      (should (equal-including-properties (read printed) properties)))))

(ert-deftest bob/gpu-snapshot-layout-keeps-geometry-not-evaluators ()
  (let* ((callback (lambda () (current-buffer)))
         (properties
          (bob/gpu-snapshot--layout-properties
           (list 'display `(space :height 2 :width 1)
                 'line-spacing 72 'face '(:height 1.2 :inherit default)
                 'window (selected-window) 'after-string
                 (propertize "label" 'display (list :eval callback))))))
    (should (equal (plist-get properties 'display) '(space :height 2 :width 1)))
    (should (= (plist-get properties 'line-spacing) 72))
    (should (equal (plist-get properties 'face) '(:height 1.2 :inherit default)))
    (should (equal (plist-get properties 'window) '(:object window)))
    (should (equal (get-text-property 0 'display (plist-get properties 'after-string))
                   '(:eval :function)))))

(provide 'bob-gpu-snapshot-tests)
;;; bob-gpu-snapshot-tests.el ends here
