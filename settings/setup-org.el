;;;; package --- Summary
;;; Commentary:
;; customo key bindings
;;
;;; code:

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; Setup org files and directories
(setq org-directory "~/Dropbox (BigPanda)/org")
(setq org-inbox-file (concat org-directory "/inbox.org"))
(setq org-someday-file (concat org-directory "/someday.org"))
(setq org-gtd-file (concat org-directory "/gtd.org"))
(setq org-tickler-file (concat org-directory "/tickler.org"))

;;;; Refile files:
(setq org-refile-targets '((org-gtd-file :maxlevel . 3)
                           (org-someday-file :level . 1)
                           (org-tickler-file :maxlevel . 2)))

;;;; Agenda config:
;; I'm setting org-agenda-files with org-agenda-file-to-front
;; untill finding a better way of doing so.

(setq org-agenda-span 'week)
(setq org-agenda-start-on-weekday nil)

;; Org Capture config:
(setq org-capture-templates '(("t" "Todo [inbox]" entry
                               (file+headline org-inbox-file "Tasks")
                               "* TODO %i%?")
                              ("T" "Tickler" entry
                               (file+headline org-tickler-file "Tickler")
                               "* %i%? \n %U")))


;; my-org-functions
(defun my-org-go-to-inbox()
  (interactive)
  (find-file org-inbox-file))

(defun my-org-go-to-todos()
  (interactive)
  (find-file org-gtd-file))

(defun my-org-go-to-tickler()
  (interactive)
  (find-file org-tickler-file))

;; Keybindings
(define-key global-map (kbd "C-c c") 'org-capture)
(define-key global-map (kbd "C-c a") 'org-agenda)
(define-key global-map (kbd "C-c o l") 'org-agenda-list)
(define-key global-map (kbd "C-c o i") 'my-org-go-to-inbox)
(define-key global-map (kbd "C-c o t") 'my-org-go-to-todos)
(define-key global-map (kbd "C-c o T") 'my-org-go-to-tickler)

(provide 'setup-org)
;;; setup-org.el ends here
