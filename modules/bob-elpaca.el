;;; bob-elpaca.el --- Elpaca maintenance commands -*- lexical-binding: t; -*-

(require 'elpaca)

;;;###autoload
(defun bob/elpaca-native-compile ()
  "Native-compile built Elpaca packages asynchronously.
Run after `elpaca-merge-all' has finished processing.
Select only sources with up-to-date bytecode.  Emacs skips current
native output, so this also fills gaps in the native compilation cache.
Do not load the compiled packages into the current session."
  (interactive)
  (unless (native-comp-available-p)
    (user-error "This Emacs does not support native compilation"))
  (unless (file-directory-p elpaca-builds-directory)
    (user-error "Elpaca build directory does not exist"))
  (native-compile-async
   elpaca-builds-directory t nil
   (lambda (file)
     (let ((bytecode (concat file "c")))
       (and (file-exists-p bytecode)
            (not (file-newer-than-file-p file bytecode))))))
  (message "Native compilation requested; see *Async-native-compile-log* for diagnostics"))

(provide 'bob-elpaca)
;;; bob-elpaca.el ends here
