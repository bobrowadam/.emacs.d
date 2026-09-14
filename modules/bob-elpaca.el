;;; bob-elpaca.el --- Elpaca maintenance commands -*- lexical-binding: t; -*-

(require 'elpaca)
(require 'seq)

;;;###autoload
(defun bob/elpaca-native-compile ()
  "Native-compile built Elpaca packages asynchronously.
Run after `elpaca-merge-all' has finished processing.
Select sources with up-to-date bytecode.  Remove native output older
than its bytecode so Emacs does not skip required recompilation.
Do not load the compiled packages into the current session."
  (interactive)
  (unless (native-comp-available-p)
    (user-error "This Emacs does not support native compilation"))
  (unless (file-directory-p elpaca-builds-directory)
    (user-error "Elpaca build directory does not exist"))
  (let ((build-dirs
         (seq-filter #'file-directory-p
                     (directory-files elpaca-builds-directory t
                                      directory-files-no-dot-files-regexp)))
        files removed)
    (setq native-comp-async-env-modifier-form
          `(setq load-path (append ',build-dirs load-path)))
    (dolist (file (directory-files-recursively
                   elpaca-builds-directory "\\.el\\'"))
      (let ((bytecode (concat file "c")))
        (when (and (file-exists-p bytecode)
                   (not (file-newer-than-file-p file bytecode)))
          (let ((native (comp-el-to-eln-filename file)))
            (when (or (not (file-exists-p native))
                      (file-newer-than-file-p bytecode native))
              (when (file-exists-p native)
                (delete-file native)
                (push native removed))
              (push file files))))))
    (if files
        (progn
          (native-compile-async (nreverse files))
          (message "Native compilation requested for %d files; removed %d stale outputs"
                   (length files) (length removed)))
      (message "Elpaca native compilation is current"))))

(provide 'bob-elpaca)
;;; bob-elpaca.el ends here
