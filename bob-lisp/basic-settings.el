(setq make-backup-files nil             
      inhibit-splash-screen t           
      require-final-newline 'ask        
      truncate-partial-width-windows 80 
      sentence-end-double-space t ; explicitly choose default        
      x-select-enable-clipboard t 
      set-mark-command-repeat-pop t 

      history-delete-duplicates t 
      comint-input-ignoredups t   

      view-read-only t ; all read-only buffers in view-mode           
      view-inhibit-help-message t ; don't tell me about it           

      delete-active-region nil ; just use <delete>                   

      gdb-many-windows t                

      epa-pinentry-mode 'loopback       
      auth-sources '("~/.authinfo.gpg") 

      ;; No more damn prompts!                                       
      dired-recursive-deletes 'always                             
      dired-recursive-copies 'always)                                
                                                                     

(setq scroll-conservatively 10                                       
      scroll-margin 2)                                               
(setq display-time-day-and-date t)                                   
(setq display-time-default-load-average nil)                         
(setq shift-select-mode nil)                                         
(display-time)
(display-battery-mode)                                               
(menu-bar-mode -1)                                                   
(when (window-system)                                                
  (setq confirm-kill-emacs 'yes-or-no-p))                            
                                                                     
(when window-system                                                  
  (tool-bar-mode -1)                                                 
  (scroll-bar-mode -1)                                               
  (tooltip-mode -1))                                                 
                                                                     
(global-subword-mode t)                                              
(global-superword-mode -1)                                           
(setq inhibit-startup-message t)                                     
(setq ring-bell-function 'ignore                                     
      visible-bell nil)                                              
(setq-default indent-tabs-mode nil)                                  
(global-set-key (kbd "M-i") 'imenu)

(delete-selection-mode 1)                                            
(set-default 'indent-tabs-mode nil)                                  
(global-auto-revert-mode 1)                                          
(auto-compression-mode t)                                            
(defalias 'yes-or-no-p 'y-or-n-p)                                    
                                                                     
;; delete char and delte word with "C-h" "C-M-h"                     
(define-key key-translation-map [?\C-h] [?\C-?])                     
(define-key key-translation-map (kbd "<f1>") (kbd "C-h"))            
;; (define-key key-translation-map (kbd "<f9>") (kbd "C-s-h"))       
(global-set-key (kbd "M-C-h") 'backward-kill-sexp)
(setq auth-sources '("~/.authinfo.gpg"))                            

;; Theme and Font
(use-package zenburn-theme
  :demand t
  :init
  (setq custom-safe-themes t)
  (defvar zenburn-override-colors-alist
  '(("zenburn-bg" . "#111111")))
  :config
  (set-frame-font "Latin Modern Mono 19")          
   (add-to-list 'default-frame-alist              
                '(font . "Latin Modern Mono 19"))
   (load-theme 'zenburn t))
 
;; Put backup files neatly away                                                 
(let ((backup-dir "~/tmp/emacs/backups")
      (auto-saves-dir "~/tmp/emacs/auto-saves/"))
  (dolist (dir (list backup-dir auto-saves-dir))
    (when (not (file-directory-p dir))
      (make-directory dir t)))
  (setq backup-directory-alist `(("." . ,backup-dir))
        auto-save-file-name-transforms `((".*" ,auto-saves-dir t))
        auto-save-list-file-prefix (concat auto-saves-dir ".saves-")
        tramp-backup-directory-alist `((".*" . ,backup-dir))
        tramp-auto-save-directory auto-saves-dir))

(setq backup-by-copying t    ; Don't delink hardlinks                           
      delete-old-versions t  ; Clean up the backups                             
      version-control t      ; Use version numbers on backups,                  
      kept-new-versions 5    ; keep some new versions                           
      kept-old-versions 2)   ; and some old ones, too

(add-to-list 'prog-mode-hook #'linum-mode)

(provide 'basic-settings)
