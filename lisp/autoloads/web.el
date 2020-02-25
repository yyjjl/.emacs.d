;; -*- lexical-binding:t -*-

(defun css//font-file-to-base64 (-file)
  (if (file-exists-p -file)
      (replace-regexp-in-string
       "\n" ""
       (shell-command-to-string (concat "cat " -file "|base64")))
    ""))

;; Colourise CSS colour literals
;;;###autoload
(defun css/convert-binary-to-code ()
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
                      (css//font-file-to-base64 (concat file-base ".eot"))
                      "') format('embedded-opentype'),\n"
                      "       url('data:application/x-font-woff;base64,"
                      (css//font-file-to-base64 (concat file-base ".woff"))
                      "') format('woff'),\n"
                      "       url('data:font/ttf;base64,"
                      (css//font-file-to-base64 (concat file-base ".ttf"))
                      "') format('truetype');"
                      "\n}")
            (concat "background:url(\"data:image/"
                    file-ext
                    ";base64,"
                    (css//font-file-to-base64 file)
                    "\") no-repeat 0 0;"))))
    (kill-new result)
    (message "css code => clipboard & yank ring")))
