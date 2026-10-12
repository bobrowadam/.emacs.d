;;; bob-invitation-flow.el --- Private VUI invitation demo -*- lexical-binding: t; -*-

;; A local prototype.  No application or service state is changed.
(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'vui)
(require 'typescript-ts-mode)

(defconst bob-invitation-flow--data-file
  (expand-file-name "bob-invitation-flow-data.json"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Revision-pinned excerpts for this walkthrough.")

(defconst bob-invitation-flow--stages
  ["Backoffice" "Bradwell" "Delivery" "Recipient"])
(defvar-local bob-invitation-flow--instance nil)
(defvar-local bob-invitation-flow--decoration nil)
(defvar-local bob-invitation-flow--previous-node nil)
(defvar-local bob-invitation-flow--node 0)
(defvar-local bob-invitation-flow--source-buffers nil)

(defface bob-invitation-flow-title
  '((t :inherit default :height 1.5 :weight bold)) "Walkthrough heading.")
(defface bob-invitation-flow-active
  '((t :foreground "#7ce3ca" :weight bold)) "Selected stage.")
(defface bob-invitation-flow-highlight
  '((t :background "#203b36" :extend t)) "Relevant source lines.")

(defun bob-invitation-flow--steps (route)
  "Return the seven steps for delivery ROUTE."
  (vconcat
   '((0 "Choose who to invite"
        "In backoffice, choose the organization, enter an email, and select a role. Automatic email starts enabled."
        "Not yet created" default)
     (1 "Save a pending invitation"
        "Bradwell checks membership and seat availability. It saves the recipient, role, token hash, and expiration before attempting email."
        "Pending: saved before email" creation))
   (pcase route
     ('manual
      '((2 "Skip the email"
           "With email off, Bradwell still returns a recipient-specific invitation link and expiration time."
           "Pending: no email requested" creation)
        (3 "Share the link manually"
           "The operator copies the link and sends it to the intended recipient, who opens it. The link is not a grant for any email address."
           "Pending: link opened" creation)))
     ('failure
      '((2 "Email fails; the invitation survives"
           "The invitation is already saved. Bradwell reports the email failure and still returns the link."
           "Pending: delivery failed" creation)
        (3 "Recover with manual sharing"
           "The operator shares the returned link manually. Email failure alone does not require a new invitation."
           "Pending: link opened" creation)))
     (_
      '((2 "Send through Customer.io"
           "Bradwell asks Customer.io to send the invitation. A successful request does not prove that it reached the inbox."
           "Pending: email requested" customerio)
        (3 "Open the invitation email"
           "The recipient follows the invitation link. Backoffice also receives a copyable link. This route assumes the email arrived."
           "Pending: link opened" signin))))
   '((3 "Sign in or create an account"
        "If needed, Bradwell routes the recipient to login or signup and preserves the invitation destination. After authentication, they return to the invitation."
        "Pending: authentication" signin)
     (1 "Validate before granting access"
        "The invitation must be pending and unexpired, with an email matching the signed-in user. Organization and membership checks must also pass."
        "Pending: checks must pass" acceptance)
     (3 "Join the organization"
        "Successful acceptance applies membership and marks the invitation accepted. The page refreshes the session and user context, then offers Continue."
        "Accepted: organization member" signin))))

(defun bob-invitation-flow--paragraph (text)
  "Wrap TEXT for the walkthrough without touching an existing buffer."
  (with-temp-buffer
    (insert text)
    (let ((fill-column 68)) (fill-region (point-min) (point-max)))
    (buffer-string)))

(defun bob-invitation-flow--clear-decoration ()
  "Release the native object owned by this walkthrough."
  (when bob-invitation-flow--decoration
    (gpu-decoration-delete bob-invitation-flow--decoration)
    (setq bob-invitation-flow--decoration nil)))

(defun bob-invitation-flow--stage-region (node)
  "Find the displayed label for NODE without moving point."
  (save-excursion
    (goto-char (point-min))
    (let ((label (format "  %s  " (aref bob-invitation-flow--stages node))))
      (when (search-forward label nil t)
        (cons (- (point) (length label)) (point))))))

(defun bob-invitation-flow--draw (&optional window)
  "Anchor an animated outline to the current stage in WINDOW."
  (setq window (or window (get-buffer-window (current-buffer) t)))
  (bob-invitation-flow--clear-decoration)
  (when (and (window-live-p window)
             (eq (window-buffer window) (current-buffer))
             (fboundp 'gpu-decoration-capabilities)
             (plist-get (gpu-decoration-capabilities (window-frame window)) :layers))
    (let* ((region (bob-invitation-flow--stage-region bob-invitation-flow--node))
           (geometry (and region (gpu-decoration-region
                                  window (car region) (cdr region) '(2 2 2 2))))
           (old-region (and bob-invitation-flow--previous-node
                            (bob-invitation-flow--stage-region
                             bob-invitation-flow--previous-node)))
           (old-geometry (and old-region (gpu-decoration-region
                                         window (car old-region) (cdr old-region)
                                         '(2 2 2 2)))))
      (when geometry
        (let* ((rect (car geometry))
               (old (or (car old-geometry) rect))
               (dx (- (nth 0 old) (nth 0 rect)))
               (dy (- (nth 1 old) (nth 1 rect)))
               (handle (gpu-decoration-create
                        (list :rect rect :clip (cadr geometry)
                              :shape 'rounded-rectangle :radius 6
                              :fill nil :stroke #x7ce3ca :stroke-width 2
                              :translate (list dx dy))
                        (window-frame window) (current-buffer))))
          (setq bob-invitation-flow--decoration handle)
          (when handle
            (gpu-decoration-anchor handle window (car region) (cdr region) '(2 2 2 2))
            (gpu-decoration-animate handle :translate-x 0 0.7 'ease-in-out)
            (gpu-decoration-animate handle :translate-y 0 0.7 'ease-in-out))))))
  (setq bob-invitation-flow--previous-node bob-invitation-flow--node))

(defun bob-invitation-flow--window-changed (window)
  "Draw the marker when WINDOW starts displaying this walkthrough."
  (with-current-buffer (window-buffer window)
    (when bob-invitation-flow--instance
      (bob-invitation-flow--draw window))))

(defun bob-invitation-flow--sources ()
  "Read the bundled snapshots, not working-tree files."
  (json-read-file bob-invitation-flow--data-file))

(defun bob-invitation-flow--source (key &optional hidden)
  "Open snapshot KEY read-only.  HIDDEN suppresses display for validation."
  (let* ((source (alist-get key (bob-invitation-flow--sources)))
         (buffer (generate-new-buffer
                  (format "*Invitation source: %s*" (alist-get 'title source))))
         (start (alist-get 'start source)))
    (push buffer bob-invitation-flow--source-buffers)
    (with-current-buffer buffer
      (insert (alist-get 'code source))
      (if (string-suffix-p ".tsx" (alist-get 'path source))
          (tsx-ts-mode)
        (typescript-ts-mode))
      (font-lock-ensure)
      (setq-local display-line-numbers-offset (1- start))
      (setq-local display-line-numbers t)
      (setq-local header-line-format
                  (format "%s  |  %s  |  lines %s–%s  |  read-only snapshot"
                          (alist-get 'path source)
                          (substring (alist-get 'revision source) 0 9)
                          start (alist-get 'end source)))
      (mapc (lambda (range)
              (goto-char (point-min))
              (forward-line (- (aref range 0) start))
              (let ((begin (point)))
                (forward-line (1+ (- (aref range 1) (aref range 0))))
                (overlay-put (make-overlay begin (point))
                             'face 'bob-invitation-flow-highlight)))
            (alist-get 'marks source))
      (goto-char (point-min))
      (when-let* ((first (seq-first (alist-get 'marks source))))
        (forward-line (- (aref first 0) start)))
      (set-buffer-modified-p nil)
      (read-only-mode 1))
    (unless hidden
      (display-buffer-in-side-window buffer '((side . right) (window-width . 0.5))))
    buffer))

(defun bob-invitation-flow--cleanup ()
  "Release the walkthrough's decorations and read-only source buffers."
  (bob-invitation-flow--clear-decoration)
  (mapc (lambda (buffer)
          (when (buffer-live-p buffer) (kill-buffer buffer)))
        bob-invitation-flow--source-buffers))

(vui-defcomponent bob-invitation-flow--view ()
  :state ((step 0) (route 'email) (playing nil))
  :on-unmount (bob-invitation-flow--cleanup)
  :render
  (let* ((steps (bob-invitation-flow--steps route))
         (current (aref steps step))
         (buffer (current-buffer)))
    (vui-use-effect (step route)
      (setq bob-invitation-flow--node (nth 0 current))
      (bob-invitation-flow--draw))
    (vui-use-effect (playing step)
      (when (and playing (< step 6))
        (let ((timer
               (run-with-timer
                6.5 nil
                (vui-with-async-context
                  (if (get-buffer-window buffer t)
                      (progn
                        (vui-set-state :step (1+ step))
                        (when (= step 5) (vui-set-state :playing nil)))
                    (vui-set-state :playing nil))))))
          (lambda () (cancel-timer timer)))))
    (vui-vstack
     :spacing 1
     (vui-text "From invitation to membership" :face 'bob-invitation-flow-title)
     (vui-text "BRA-1653 · Backoffice · Revision 3eb61633b" :face 'shadow)
     (apply #'vui-hstack :spacing 2
            (mapcar
             (lambda (item)
               (vui-button
                (cdr item) :face (if (eq route (car item))
                                    'bob-invitation-flow-active 'button)
                :on-click (lambda ()
                            (vui-set-state :route (car item))
                            (vui-set-state :step 0)
                            (vui-set-state :playing nil))))
             '((email . "Email on") (manual . "Email off")
               (failure . "Delivery fails"))))
     (apply #'vui-hstack :spacing 1
            (cl-loop for label across bob-invitation-flow--stages
                     for node from 0
                     collect (vui-text (format "  %s  " label)
                                       :face (if (= node (nth 0 current))
                                                 'bob-invitation-flow-active
                                               'shadow))))
     (vui-text (format "Step %d of 7 · %s" (1+ step) (nth 1 current))
               :face 'bold)
     (vui-text (bob-invitation-flow--paragraph (nth 2 current)))
     (vui-text (nth 3 current) :face 'bob-invitation-flow-active)
     (vui-hstack
      :spacing 2
      (vui-button "Back" :disabled (= step 0)
                  :on-click (lambda () (vui-set-state :playing nil)
                              (vui-set-state :step (1- step))))
      (vui-button (if playing "Pause" "Play")
                  :on-click (lambda ()
                              (when (= step 6) (vui-set-state :step 0))
                              (vui-set-state :playing (not playing))))
      (vui-button "Next" :disabled (= step 6)
                  :on-click (lambda () (vui-set-state :playing nil)
                              (vui-set-state :step (1+ step))))
      (vui-button "Replay" :on-click (lambda ()
                                       (vui-set-state :step 0)
                                       (vui-set-state :playing t))))
     (vui-button "Read this step's source →"
                 :on-click (lambda ()
                             (vui-set-state :playing nil)
                             (bob-invitation-flow--source (nth 4 current))))
     (vui-text (bob-invitation-flow--paragraph
                "The link is not membership. Sign-in and a matching email are required. Failed membership checks stop acceptance. Dashboard invitations keep their existing email-only behavior.")
               :face 'shadow)
     (vui-text "Local explanation only. No invitations are sent. Playback: 6.5 seconds per step."
               :face 'shadow))))

(defun bob-invitation-flow (&optional hidden)
  "Open the private native invitation walkthrough.
With HIDDEN non-nil, prepare its buffer without displaying it."
  (interactive)
  (let ((buffer (get-buffer-create "*Invitation flow*")))
    (with-current-buffer buffer
      (unless bob-invitation-flow--instance
        (vui-mode)
        (setq-local truncate-lines nil)
        (setq bob-invitation-flow--instance
              (vui-mount-inline (vui-component 'bob-invitation-flow--view)
                                (point-min)))
        (add-hook 'kill-buffer-hook #'bob-invitation-flow--cleanup nil t)
        (add-hook 'change-major-mode-hook #'bob-invitation-flow--cleanup nil t)
        (add-hook 'window-buffer-change-functions
                  #'bob-invitation-flow--window-changed nil t)))
    (unless hidden
      (pop-to-buffer buffer)
      (bob-invitation-flow--draw))
    buffer))

(provide 'bob-invitation-flow)
;;; bob-invitation-flow.el ends here
