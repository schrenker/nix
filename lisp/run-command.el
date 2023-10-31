(defun schrenker/run-command-recipes-local ()
  (list
   (when-let ((file-name (buffer-file-name))
              (go (equal (file-name-extension file-name) "go"))
              (yaegi-path (or
                           (executable-find "yaegi")
                           (when-let ((path (car (cl-member "yaegi" (split-string (getenv "PATH") ":") :test #'string-match-p))))
                             (concat path "/yaegi")))))
     (list
      :command-name "yaegi"
      :command-line (format "direnv exec . yaegi %s" file-name)
      :display
      (format "Run `%s' go file with yaegi"
              (file-name-nondirectory file-name))))

   (when-let ((file-name (buffer-file-name))
              (dos2unix-path (executable-find "dos2unix")))
     (list
      :command-name "dos2unix"
      :command-line (format "dos2unix %s" file-name)
      :display
      (format "Run dos2unix on file `%s'" (file-name-nondirectory file-name))))))

(add-to-list 'run-command-recipes 'schrenker/run-command-recipes-local)
