;; -*- lexical-binding:t -*-

(defun ymacs-web//font-file-to-base64 (-file)
  (if (file-exists-p -file)
      (replace-regexp-in-string
       "\n" ""
       (shell-command-to-string (concat "cat " -file "|base64")))
    ""))

;;;###autoload
(defun ymacs-web/load-in-repl ()
  (interactive)
  (cond
   ((file-remote-p default-directory)
    (message "Not support in remove sever !"))
   ((not (executable-find "nodejs"))
    (message "Executable `nodejs' not found !"))
   ((not (buffer-file-name))
    (message "Buffer has no file !"))
   (t
    (let ((file (buffer-file-name)))
      (ymacs-term//exec-program-reuse-buffer
       (concat "Node:" (buffer-name))
       "node" (list "-l" (or file ""))
       :callback
       (lambda ()
         (ymacs-term//send-string (format ".load %s\n" file))))))))

;; Colourise CSS colour literals
;;;###autoload
(defun ymacs-web/convert-binary-to-code ()
  "Convert binary (image, font...) into css."
  (interactive)
  (let* ((file (completing-read "The image path: " #'read-file-name-internal))
         (file-ext (file-name-extension file))
         (file-base (file-name-sans-extension file))
         (result
          (if (member file-ext '("ttf" "eot" "woff"))
              (concat "@font-face {\n"
                      "  font-family: familarName;\n"
                      "  src: url('data:font/eot;base64,"
                      (ymacs-web//font-file-to-base64 (concat file-base ".eot"))
                      "') format('embedded-opentype'),\n"
                      "       url('data:application/x-font-woff;base64,"
                      (ymacs-web//font-file-to-base64 (concat file-base ".woff"))
                      "') format('woff'),\n"
                      "       url('data:font/ttf;base64,"
                      (ymacs-web//font-file-to-base64 (concat file-base ".ttf"))
                      "') format('truetype');"
                      "\n}")
            (concat "background:url(\"data:image/"
                    file-ext
                    ";base64,"
                    (ymacs-web//font-file-to-base64 file)
                    "\") no-repeat 0 0;"))))
    (kill-new result)
    (message "css code => clipboard & yank ring")))
