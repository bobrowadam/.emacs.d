;;; bob-gpu-snapshot.el --- Silent GPU glitch snapshots -*- lexical-binding: t; -*-

;; Record what Bmacs has rendered, plus decoration and window state, before any
;; message or redisplay-triggering UI, so a transient glitch survives capture.
(require 'cl-lib)
(require 'gpu)
(require 'pp)
(require 'seq)

(defconst bob/gpu-snapshot-directory "/tmp/gpu-glitch/"
  "Parent directory for timestamped snapshots.")

(defconst bob/gpu-snapshot-readme
  "Bmacs GPU glitch snapshot, saved by `bob/gpu-snapshot' (key C in Mentat).

frame-N-screen.png     What macOS showed in frame N's screen area when the key
                       was pressed, captured first.  Windows or overlays on top
                       of the frame appear too.  Compare the others with this.
frame-N-metal.png      Bmacs's Metal texture readback: text and cursor, without
                       Core Animation decoration layers.  Only the screen
                       image shows decorations; state.eld records them.
state.eld              Elisp plist: frame geometry and scale, windows, cursors,
                       child frames, and tracked plus untracked native
                       decorations (:untracked lists native IDs that Lisp lost).
"
  "README written into each snapshot directory.")

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

(defun bob/gpu-snapshot--screen (frame file)
  "Save the on-screen pixels of FRAME to FILE and return FILE, or nil.
Capture by region, because Bmacs's `window-id' is not a macOS window
number; overlapping windows and overlays appear as they do on screen."
  (pcase-let ((`(,left ,top ,right ,bottom) (frame-edges frame 'outer-edges)))
    (and (eq 0 (call-process "/usr/sbin/screencapture" nil nil nil "-x" "-R"
                             (format "%d,%d,%d,%d" left top (- right left) (- bottom top))
                             file))
         file)))

(defun bob/gpu-snapshot--frame (frame directory index screen)
  "Capture FRAME into DIRECTORY using INDEX, and return its state.
SCREEN is the on-screen capture already taken for FRAME."
  (let ((metal (expand-file-name (format "frame-%d-metal.png" index) directory)))
    (list :frame (format "%s" frame)
          :pixel-size (list (frame-pixel-width frame) (frame-pixel-height frame))
          :position (frame-position frame)
          :scale (frame-scale-factor frame)
          :monitor (assq-delete-all 'frames (frame-monitor-attributes frame))
          :screen screen
          :metal (and (ignore-errors (gpu-capture-frame metal frame)) metal)
          :transition (ignore-errors (gpu-transition-active-p frame))
          :windows (mapcar #'bob/gpu-snapshot--window (window-list frame t))
          :decorations (bob/gpu-snapshot--decorations frame))))

;;;###autoload
(defun bob/gpu-snapshot ()
  "Save screen pixels, a Metal readback and display state for a glitch.
The screen captures come first, before readbacks can trigger a redraw.
Put the snapshot directory on the kill ring, to paste into a prompt, and
report it once everything is saved."
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
    (let ((screens (cl-loop for frame in frames
                            for index from 1
                            collect (bob/gpu-snapshot--screen
                                     frame (expand-file-name
                                            (format "frame-%d-screen.png" index)
                                            directory)))))
      (setq state (plist-put state :frames
                             (cl-loop for frame in frames
                                      for screen in screens
                                      for index from 1
                                      collect (bob/gpu-snapshot--frame
                                               frame directory index screen)))))
    ;; A non-string VISIT keeps `write-region' from echoing "Wrote ...".
    (write-region (pp-to-string state) nil
                  (expand-file-name "state.eld" directory) nil 'silent)
    (write-region bob/gpu-snapshot-readme nil
                  (expand-file-name "README.txt" directory) nil 'silent)
    (kill-new directory)
    (message "GPU snapshot saved to %s (path copied)" directory)))

(provide 'bob-gpu-snapshot)
;;; bob-gpu-snapshot.el ends here
