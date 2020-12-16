;; -*- lexical-binding: t; -*-

(defun ymacs-cpp-clangd//filter-arguments (-args)
  (let (args arg)
    (while (setq arg (pop -args))
      (if (string= arg "-o")
          (pop -args)
        (push arg args)))
    (nreverse args)))

(defsubst ymacs-cpp-clangd//dot-clangd-path (&optional -directory)
  (cl-loop for name in '("compile_flags.txt" ".clangd")
           for dir = (locate-dominating-file name (or -directory default-directory))
           when dir
           return (expand-file-name name dir)))

(defun ymacs-cpp-clangd//buffer-compile-command (&optional -preprocess-only-p)
  (cl-assert (buffer-file-name) nil "non-file buffer")

  (let ((command
         (or
          (when-let ((cdb-path (ignore-errors (ymacs-cpp-cmake//cdb-path)))
                     (cdb (and (file-exists-p cdb-path)
                               (json-parse-string (read-file-content! cdb-path)))))
            (cl-assert (sequencep cdb) nil "Invalid compile_commands.json")
            (string-join
             (ymacs-cpp-clangd//filter-arguments
              (split-string
               (cl-loop for opt across cdb
                        for entry-file = (file-name-sans-extension (or (file-truename (gethash "file" opt)) ""))
                        for buffer-file = (file-name-sans-extension (or (buffer-file-name) ""))
                        when (equal entry-file buffer-file)
                        return (gethash "command" opt))))
             " "))

          (when-let ((dot-clangd (ymacs-cpp-clangd//dot-clangd-path))
                     (flags (and (file-exists-p dot-clangd)
                                 (replace-regexp-in-string "\n" " " (read-file-content! dot-clangd) t t))))
            (format "g++ %s %s" flags (buffer-file-name))))))

    (if -preprocess-only-p
        (concat command " -E -xc++ -C -")
      command)))
