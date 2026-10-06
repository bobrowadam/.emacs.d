;;; bob-mentat-tests.el --- Codex usage presentation tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Load only the presentation definitions, without running private startup.

;;; Code:
(require 'ert)
(require 'cl-lib)
(require 'ansi-color)
(require 'mentat)
(require 'mentat-footer)

(with-temp-buffer
  (insert-file-contents
   (expand-file-name "bob-mentat.el"
                     (file-name-directory (or load-file-name buffer-file-name))))
  (goto-char (point-min))
  (condition-case nil
      (while t
        (let ((form (read (current-buffer))))
          (when (and (eq (car-safe form) 'defun)
                     (memq (cadr form) '(bob/mentat-codex-window-usage
                                        bob/mentat-codex-usage)))
            (eval form t))))
    (end-of-file nil)))

(ert-deftest bob/mentat-codex-rpc-status-shows-both-windows ()
  "Replay recorded status through the normalized UI path into the footer."
  (let* ((status "\x1b[0mCodex Pro · 2 reset credits\n\n5h   ███░░░░░░░  27.5% used   resets 18:00\n7d   ████████░░  80.0% used   resets Mon 12:00")
         (event `((type . "extension_ui_request") (id . "codex-status")
                  (method . "setStatus") (statusKey . "codex")
                  (statusText . ,status)))
         (view (mentat--buffer-create
                (generate-new-buffer-name " *bob-codex-test*")))
         (buffer (mentat--buffer-buffer view)))
    (unwind-protect
        (with-current-buffer buffer
          (setq mentat--buffer-model-provider "openai-codex")
          (setq-local mentat-mode-line-extra-functions '(bob/mentat-codex-usage))
          (let ((wire (json-parse-string (json-serialize event)
                                        :object-type 'alist :array-type 'list)))
            (should (equal (mentat--event-normalize event)
                           (mentat--event-normalize wire)))
            (mentat--buffer-handle-event view wire))
          (let* ((segment (bob/mentat-codex-usage))
                 (weekly (string-match "wk" segment)))
            (should (equal (substring-no-properties segment)
                           "Codex 5h 72%% left · wk 20%% left"))
            (should (eq 'warning (get-text-property weekly 'face segment)))
            (should (string-match-p "resets Mon"
                                    (get-text-property weekly 'help-echo segment))))
          (cl-letf (((symbol-function 'window-buffer) (lambda (_) buffer))
                    ((symbol-function 'window-body-width) (lambda (&rest _) 160))
                    ((symbol-function 'format-mode-line)
                     (lambda (segment &rest _)
                       (replace-regexp-in-string "%%" "%" segment t t))))
            (should (string-match-p "Codex 5h 72% left · wk 20% left"
                                    (nth 2 (mentat--footer-lines nil)))))
          (setq mentat--buffer-model-provider "azure")
          (should-not (bob/mentat-codex-usage))
          (setq mentat--buffer-model-provider "openai-codex")
          (mentat--buffer-handle-event
           view '((type . "extension_ui_request") (id . "clear")
                  (method . "setStatus") (statusKey . "codex")))
          (should-not (bob/mentat-codex-usage)))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest bob/mentat-codex-compact-blocked-and-unavailable-status ()
  "Handle compact and blocked windows without confusing used with remaining."
  (let ((mentat--buffer-model-provider "openai-codex")
        status)
    (cl-letf (((symbol-function 'mentat-extension-status) (lambda (_) status)))
      (setq status "codex 90% 5h 8% wk 2 reset |")
      (should (equal (substring-no-properties (bob/mentat-codex-usage))
                     "Codex 5h 90%% left · wk 8%% left"))
      (should (eq 'error (get-text-property
                          (string-match "wk" (bob/mentat-codex-usage))
                          'face (bob/mentat-codex-usage))))
      (setq status "codex blocked 5h until 18:00 |")
      (should (equal (substring-no-properties (bob/mentat-codex-usage))
                     "Codex blocked 5h until 18:00"))
      (setq status "Codex usage unavailable")
      (should (equal (substring-no-properties (bob/mentat-codex-usage))
                     "Codex usage unavailable"))
      (setq status "5h 100% used resets 18:00\n7d 0% used resets Mon")
      (should (equal (substring-no-properties (bob/mentat-codex-usage))
                     "Codex 5h 0%% left · wk 100%% left")))))

(provide 'bob-mentat-tests)
;;; bob-mentat-tests.el ends here
