(require 'org)

(setq-default indent-tabs-mode nil)
(setq org-display-inline-images t)
(setq org-redisplay-inline-images t)
(setq org-startup-with-inline-images "inlineimages")

(setq default-frame-alist
      (append (list '(width . 72) '(height . 40))))

(setq org-confirm-elisp-link-function nil)
      
				  
(global-set-key [(control z)]         'undo)
(global-set-key "\C-x\C-x"            'execute-extended-command)

(set-frame-font "Roboto Mono Light 18")

(set-frame-parameter (selected-frame) 'internal-border-width 20)
(setq x-underline-at-descent-line t)
(setq initial-major-mode 'text-mode)
(setq-default line-spacing 0)
(set-default 'cursor-type  '(hbar . 2))
(blink-cursor-mode 0)
(fringe-mode '(0 . 0))

(setq frame-background-mode 'light)
(set-background-color "#ffffff")
(set-foreground-color "#666666")

(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-message t)   ;; Show/hide startup page
(setq initial-scratch-message nil) ;; Show/hide *scratch* buffer message
;; (menu-bar-mode 0)                  ;; Show/hide menubar
(tool-bar-mode 0)                  ;; Show/hide toolbar
(tooltip-mode  0)                  ;; Show/hide tooltip
(scroll-bar-mode 0)                ;; Show/hide scrollbar



(defun mode-line-render (left right)
  "Return a string of `window-width' length containing left, and
   right aligned respectively."
  (let* ((available-width (- (window-total-width) (length left) )))
    (format (format "%%s %%%ds" available-width) left right)))


(setq-default header-line-format
  '(:eval (mode-line-render

   (format-mode-line
    (list
     (propertize "File " 'face `(:weight regular))
     "%b "
     '(:eval (if (and buffer-file-name (buffer-modified-p))
         (propertize "(modified)" 
		     'face `(:weight light
			     :foreground "#aaaaaa"))))))
   
   (format-mode-line
    (propertize "%3l:%2c "
	'face `(:weight light :foreground "#aaaaaa"))))))

(set-face-attribute 'region nil
		    :background "#f0f0f0")
(set-face-attribute 'highlight nil
		    :foreground "black"
		    :background "#f0f0f0")
(set-face-attribute 'org-level-1 nil
		    :foreground "black"
		    :weight 'regular)
(set-face-attribute 'org-link nil
		    :underline nil
		    :foreground "dark blue")
(set-face-attribute 'org-verbatim nil
		    :foreground "dark blue")
(set-face-attribute 'bold nil
 		    :foreground "black"
		    :weight 'regular)


(setq-default mode-line-format   "")

(set-face-attribute 'header-line nil
;;                    :weight 'regular
		    :height 140
                    :underline "black"
                    :foreground "black"
		    :background "white"
                    :box `(:line-width 3 :color "white" :style nil))
(set-face-attribute 'mode-line nil
                    :height 10
                    :underline "black"
                    :background "white"
		                :foreground "white"
                    :box nil)
(set-face-attribute 'mode-line-inactive nil
                    :box nil
                    :inherit 'mode-line)
(set-face-attribute 'mode-line-buffer-id nil 
                    :weight 'light)
(setq org-hide-emphasis-markers t)

;; Delete with C-h;
(define-key key-translation-map [?\C-h] [?\C-?])
(define-key key-translation-map (kbd "<f1>") (kbd "C-h"))
