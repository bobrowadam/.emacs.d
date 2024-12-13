;; Match theme color early on (smoother transition).
;; Theme loaded in features/ui.el.
(add-to-list 'default-frame-alist '(background-color . "#212121"))

;; https://github.com/hlissner/doom-emacs/blob/58af4aef56469f3f495129b4e7d947553f420fca/core/core.el#L200
(unless (daemonp)
  (advice-add #'display-startup-echo-area-message :override #'ignore))

;; https://github.com/hlissner/doom-emacs/blob/58af4aef56469f3f495129b4e7d947553f420fca/core/core.el#L323
(setq frame-inhibit-implied-resize t)

;; https://github.com/hlissner/doom-emacs/blob/58af4aef56469f3f495129b4e7d947553f420fca/core/core.el#L331
(setq inhibit-compacting-font-caches t)

;; https://github.com/hlissner/doom-emacs/blob/58af4aef56469f3f495129b4e7d947553f420fca/core/core.el#L205
(setq idle-update-delay 1.0)

;; Don't want a mode line while loading init.
(setq mode-line-format nil)

;; No scrollbar by default.
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; No nenubar by default.
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))

;; No toolbar by default.
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; No tooltip by default.
(when (fboundp 'tooltip-mode)
  (tooltip-mode -1))

;; No Alarms by default.
(setq ring-bell-function 'ignore)

(setq initial-scratch-message nil)
(setq inhibit-startup-screen t)

(defface init-title
  '((t :inherit info-title-3 :height 300))
  "A face For the initial Emacs title.")

(setq gc-cons-percentage-before-init gc-cons-percentage)
(setq gc-cons-threshold-before-init gc-cons-threshold)
(setq gc-cons-percentage 0.6)
(setq gc-cons-threshold (* gc-cons-threshold 1000))
(add-hook 'emacs-startup-hook
          (lambda ()
            (insert (propertize "M-x"
                                'face '(init-title)))
            (insert "\n\n ")
            (insert (propertize (format "Ready in %s with %d garbage collections.\nGC elapsed: %s"
                                        (format "%.2f seconds"
                                                (float-time
                                                 (time-subtract after-init-time before-init-time)))
                                        gcs-done
                                        (format "%.2f seconds" gc-elapsed))
                                'face '(info-title-4)))
            (setq gc-cons-percentage gc-cons-percentage-before-init)
            (setq gc-cons-threshold gc-cons-threshold-before-init)))
