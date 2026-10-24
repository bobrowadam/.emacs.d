;;; bob-gpu-snapshot.el --- Silent GPU glitch snapshots -*- lexical-binding: t; -*-

;; Record what Bmacs has rendered, plus decoration and window state, without
;; messages or redisplay-triggering UI, so a transient glitch survives capture.
(require 'cl-lib)
(require 'gpu)
(require 'pp)
(require 'seq)

(defconst bob/gpu-snapshot-directory "/tmp/gpu-glitch/"
  "Parent directory for timestamped snapshots.")

(defun bob/gpu-snapshot--window (window)
  "Describe WINDOW's geometry, point, and cursor."
  (let* ((buffer (window-buffer window))
         (point (window-point window))
         (posn (and (pos-visible-in-window-p point window)
                    (posn-at-point point window))))
    (list :window (format "%s" window) :buffer (buffer-name buffer)
          :selected (eq window (selected-window))
          :edges (window-inside-pixel-edges window)
          :start (window-start window) :end (window-end window)
          :vscroll (window-vscroll window t) :point point
          :cursor-type (buffer-local-value 'cursor-type buffer)
          :cursor (and posn (list (posn-x-y posn) (posn-object-width-height posn))))))

(defun bob/gpu-snapshot--decorations (frame)
  "Describe tracked decorations on FRAME and any untracked native ones."
  (let* ((tracked (seq-filter (lambda (d) (eq (gpu--decoration-frame d) frame))
                              gpu--decorations))
         (ids (mapcar #'gpu--decoration-id tracked))
         (limit (+ 200 (apply #'max 0 ids))))
    (list :tracked
          (mapcar (lambda (d)
                    (list :id (gpu--decoration-id d)
                          :owner (format "%s" (gpu--decoration-owner d))
                          :properties (gpu--decoration-properties d)
                          :native (gpu--decoration-state (gpu--decoration-id d) frame)))
                  tracked)
          :untracked
          (cl-loop for id from 1 to limit
                   for state = (and (not (memq id ids))
                                    (gpu--decoration-state id frame))
                   when state collect (cons id state)))))

(defun bob/gpu-snapshot--frame (frame directory index)
  "Capture FRAME into DIRECTORY using INDEX, and return its state."
  (let ((metal (expand-file-name (format "frame-%d-metal.png" index) directory))
        (composite (expand-file-name (format "frame-%d-composite.png" index) directory)))
    (list :frame (format "%s" frame)
          :pixel-size (list (frame-pixel-width frame) (frame-pixel-height frame))
          :position (frame-position frame)
          :scale (frame-scale-factor frame)
          :monitor (assq-delete-all 'frames (frame-monitor-attributes frame))
          :metal (and (ignore-errors (gpu-capture-frame metal frame)) metal)
          :composite (and (ignore-errors (gpu-capture-frame composite frame t)) composite)
          :transition (ignore-errors (gpu-transition-active-p frame))
          :windows (mapcar #'bob/gpu-snapshot--window (window-list frame t))
          :decorations (bob/gpu-snapshot--decorations frame))))

;;;###autoload
(defun bob/gpu-snapshot ()
  "Silently save GPU readbacks and display state for a visible glitch.
Take a macOS window screenshot first; this only reads Emacs state."
  (interactive)
  (let* ((directory (file-name-as-directory
                     (expand-file-name (format-time-string "%Y%m%d-%H%M%S")
                                       bob/gpu-snapshot-directory)))
         (frames (seq-filter (lambda (frame)
                               (and (frame-visible-p frame)
                                    (not (frame-parameter frame 'parent-frame))
                                    (gpu-decoration-supported-p frame)))
                             (frame-list)))
         (state (list :time (format-time-string "%FT%T%z")
                      :last-command last-command
                      :gpu-cursor-mode (bound-and-true-p gpu-cursor-mode)
                      :child-frames
                      (cl-loop for frame in (frame-list)
                               when (frame-parameter frame 'parent-frame)
                               collect (list (format "%s" frame)
                                             :visible (frame-visible-p frame)
                                             :position (frame-position frame)
                                             :size (list (frame-pixel-width frame)
                                                         (frame-pixel-height frame))))
                      :frames nil)))
    (make-directory directory t)
    (setq state (plist-put state :frames
                           (cl-loop for frame in frames
                                    for index from 1
                                    collect (bob/gpu-snapshot--frame frame directory index))))
    ;; A non-string VISIT keeps `write-region' from echoing "Wrote ...".
    (write-region (pp-to-string state) nil
                  (expand-file-name "state.eld" directory) nil 'silent)))

(provide 'bob-gpu-snapshot)
;;; bob-gpu-snapshot.el ends here
