;;; bob-background-sounds.el --- Control macOS Background Sounds -*- lexical-binding: t; -*-

;;; Commentary:

;; Turn macOS Background Sounds on and off from Emacs.  The setting
;; lives in the `com.apple.ComfortSounds' defaults domain.  The
;; `heard' accessibility agent plays the sounds and reads the setting
;; only at startup, so each change kills it and lets launchd relaunch
;; it.  System Integrity Protection blocks `launchctl kickstart' here.
;;
;; Choosing a sound runs a Shortcuts shortcut, named by
;; `bob/background-sounds-shortcut', that passes its text input to the
;; "Change Background Sound" action.  Shortcuts downloads sounds that
;; are not yet installed.

;;; Code:

(defgroup bob-background-sounds nil
  "Control macOS Background Sounds."
  :group 'external)

(defcustom bob/background-sounds-shortcut "Set Background Sound"
  "Name of the shortcut that changes the Background Sound.
The shortcut must pass its text input to \"Change Background Sound\"."
  :type 'string)

(defconst bob/background-sounds-names
  '("Balanced Noise" "Bright Noise" "Dark Noise" "Ocean" "Rain" "Stream"
    "Night" "Fire" "Babble" "Steam" "Airplane" "Boat" "Bus" "Train"
    "Rain On Roof" "Quiet Night")
  "Background Sound names as macOS shows them.")

(defconst bob/background-sounds-domain "com.apple.ComfortSounds"
  "Defaults domain for macOS Background Sounds.")

(defun bob/background-sounds--run (program &rest args)
  "Run PROGRAM with ARGS and return its trimmed output.
Signal an error when PROGRAM exits with a nonzero status."
  (with-temp-buffer
    (let ((status (apply #'call-process program nil t nil args)))
      (unless (eql status 0)
        (error "%s failed: %s" program (string-trim (buffer-string))))
      (string-trim (buffer-string)))))

(defun bob/background-sounds-enabled-p ()
  "Return non-nil when macOS Background Sounds are on."
  (equal (ignore-errors
           (bob/background-sounds--run
            "defaults" "read" bob/background-sounds-domain
            "comfortSoundsEnabled"))
         "1"))

(defun bob/background-sounds-set (enable)
  "Turn macOS Background Sounds on when ENABLE is non-nil, else off."
  (bob/background-sounds--run
   "defaults" "write" bob/background-sounds-domain
   "comfortSoundsEnabled" "-bool" (if enable "true" "false"))
  (bob/background-sounds--run "killall" "heard")
  (message "Background Sounds %s" (if enable "on" "off")))

;;;###autoload
(defun bob/background-sounds-on ()
  "Turn macOS Background Sounds on."
  (interactive)
  (bob/background-sounds-set t))

;;;###autoload
(defun bob/background-sounds-off ()
  "Turn macOS Background Sounds off."
  (interactive)
  (bob/background-sounds-set nil))

;;;###autoload
(defun bob/background-sounds-toggle ()
  "Toggle macOS Background Sounds."
  (interactive)
  (bob/background-sounds-set (not (bob/background-sounds-enabled-p))))

;;;###autoload
(defun bob/background-sounds-choose (sound)
  "Change the macOS Background Sound to SOUND."
  (interactive
   (list (completing-read "Background Sound: "
                          bob/background-sounds-names nil t)))
  (let ((input (make-temp-file "background-sound" nil ".txt")))
    (write-region sound nil input nil 'silent)
    (make-process
     :name "background-sounds-choose"
     :buffer (generate-new-buffer " *background-sounds-choose*")
     :command (list "shortcuts" "run" bob/background-sounds-shortcut
                    "--input-path" input)
     :sentinel
     (lambda (process _event)
       (unless (process-live-p process)
         (delete-file input)
         (let ((buffer (process-buffer process)))
           (if (eql (process-exit-status process) 0)
               (message "Background Sound: %s" sound)
             (message "Background Sound shortcut failed: %s"
                      (string-trim (with-current-buffer buffer
                                     (buffer-string)))))
           (kill-buffer buffer)))))))

(provide 'bob-background-sounds)
;;; bob-background-sounds.el ends here
