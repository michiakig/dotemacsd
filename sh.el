(defun squish (dir)
  "Remove one level of duplicated directories
   e.g. flattens /home/user/foo/foo/ to /home/user/foo
   (only if foo/foo is the only thing in foo)"
  (when (file-exists-p dir)
    (let* ((contents (nodots (directory-files dir)))
           (name (file-name-nondirectory dir))
           (from (concat dir "/" name))
           (tmp (concat "/tmp/" (make-temp-name name))))
      (if (and (= 1 (length contents))
               (string-equal name (first contents)))
          (progn
            (copy-directory from tmp t t nil)
            (delete-directory dir t)
            (copy-directory tmp dir t t nil)
            (delete-directory tmp t))
        (message (format "Directory not empty or doesn't seem to be a dupe? %s %s"
                         contents dir))))))
