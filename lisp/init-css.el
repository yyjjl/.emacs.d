;; Colorize strings that represent colors
(package|require 'rainbow-mode)
(package|require 'css-eldoc)
(when js2|has-web-beautify-p
  (package|require 'web-beautify))



(defun font-file-to-base64 (file)
  (if (file-exists-p file)
      (replace-regexp-in-string "\n" "" (shell-command-to-string
                                         (concat "cat " file "|base64")))
    ""))

;; Colourise CSS colour literals
(defun css|convert-binary-to-code ()
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
                      (font-file-to-base64 (concat file-base ".eot"))
                      "') format('embedded-opentype'),\n"
                      "       url('data:application/x-font-woff;base64,"
                      (font-file-to-base64 (concat file-base ".woff"))
                      "') format('woff'),\n"
                      "       url('data:font/ttf;base64,"
                      (font-file-to-base64 (concat file-base ".ttf"))
                      "') format('truetype');"
                      "\n}")
            (concat "background:url(\"data:image/"
                    file-ext
                    ";base64,"
                    (font-file-to-base64 file)
                    "\") no-repeat 0 0;"))))
    (kill-new result)
    (message "css code => clipboard & yank ring")))

(defun css|imenu-make-index ()
  (save-excursion
    (imenu--generic-function '((nil "^ *\\([^ ]+\\) *{ *$" 1)))))

(defhook css|setup (css-mode-hook sass-mode-hook)
  (rainbow-mode 1)
  (when (require 'css-eldoc nil t)
    (turn-on-css-eldoc))
  (unless (buffer-temporary-p)
    (setq imenu-create-index-function 'css|imenu-make-index)))

(with-eval-after-load 'css-mode
  (define-key css-mode-map (kbd "C-c b") #'web-beautify-css))

(provide 'init-css)
