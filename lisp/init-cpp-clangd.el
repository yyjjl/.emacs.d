;; -*- lexical-binding:t -*-

(add-to-list 'cpp-buffer-command-functions #'cpp-clangd//buffer-compile-command)

(setq cpp-lsp-checker (lambda () (file-exists-p (cpp-clangd//dot-clangd-path))))
(setq cpp-expand-macro-function (lambda () (cpp-clangd//buffer-compile-command t)))

(defsubst cpp-clangd//dot-clangd-path (&optional -directory)
  (->> (or -directory default-directory)
       (locate-dominating-file "compile_flags.txt")
       (expand-file-name "compile_flags.txt")))

(defun cpp-clangd//buffer-compile-command (&optional -preprocess-only-p)
  (cl-assert (buffer-file-name) nil "non-file buffer")

  (let ((command
         (or
          (when-let ((cdb-path (cpp-cmake//cdb-path))
                     (cdb (and (file-exists-p cdb-path)
                               (json-parse-string (read-file-content! cdb-path)))))
            (cl-assert (sequencep cdb) nil "Invalid compile_commands.json")
            (cl-loop for opt across cdb
                     when (equal (file-truename (gethash "file" opt)) (buffer-file-name))
                     return (gethash "command" opt)))

          (when-let ((dot-clangd (cpp-clangd//dot-clangd-path))
                     (flags (and (file-exists-p dot-clangd)
                                 (s-replace "\n" " " (read-file-content! dot-clangd)))))
            (format "g++ %s %s" flags (buffer-file-name))))))

    (if -preprocess-only-p
        (concat command " -E -xc++ -C -")
      command)))

(config! lsp-mode
  :config
  (setq lsp-disabled-clients '(ccls))
  (setq lsp-clients-clangd-args '("--all-scopes-completion" "--clang-tidy" "--suggest-missing-includes")))

(provide 'init-cpp-clangd)
