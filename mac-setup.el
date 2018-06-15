;; First time environment setup in mac

(install-brew)
(set-path)
(install-nodejs "8")

(defun install-brew ()
  (unless (s-contains\? "brew" (shell-command-to-string "which brew"))
    (call-process "ruby" "-e" "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)")))

(defun set-path ()
  (let ((path (shell-command-to-string "echo $PATH"))
        (wanted "/usr/local/bin" ))
    (if (s-contains\? wanted path)
        (message "PATH contains wanted pathes")
      (message "PATH missing wanted pathes"))))

(defun my/extract-version-from-string (string)
  (replace-regexp-in-string "v\\|\n\\'" ""  string))

(defun install-nodejs (node-version)
 (let ((current-node-version  (my/extract-version-from-string  (shell-command-to-string "node --version"))))
   (if (equal current-node-version node-version)
       (message "Node already installed. skipping")
     (progn (message (format "Installing node %s" node-version))
            (shell-command-to-string (format "brew install node@%s" node-version))
            ()))))
