;; -*- lexical-binding: t; -*-

(defsubst ymacs-cpp-clangd//dot-clangd-path (&optional -directory)
  (->> (or -directory default-directory)
       (locate-dominating-file "compile_flags.txt")
       (expand-file-name "compile_flags.txt")))

(defun ymacs-cpp-clangd//buffer-compile-command (&optional -preprocess-only-p)
  (cl-assert (buffer-file-name) nil "non-file buffer")

  (let ((command
         (or
          (when-let ((cdb-path (ignore-errors (ymacs-cpp-cmake//cdb-path)))
                     (cdb (and (file-exists-p cdb-path)
                               (json-parse-string (read-file-content! cdb-path)))))
            (cl-assert (sequencep cdb) nil "Invalid compile_commands.json")
            (cl-loop for opt across cdb
                     when (equal (file-truename (gethash "file" opt)) (buffer-file-name))
                     return (gethash "command" opt)))

          (when-let ((dot-clangd (ymacs-cpp-clangd//dot-clangd-path))
                     (flags (and (file-exists-p dot-clangd)
                                 (replace-regexp-in-string "\n" " " (read-file-content! dot-clangd) t t))))
            (format "g++ %s %s" flags (buffer-file-name))))))

    (if -preprocess-only-p
        (concat command " -E -xc++ -C -")
      command)))
