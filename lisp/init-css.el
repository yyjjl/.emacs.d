;; Colourise CSS colour literals
;; web-mode does not like rainbow-mode
(defun convert-binary-to-css-code ()
  "Convert binary (image, font...) into css."
  (interactive)
  (let (str
        rlt
        (file (read-file-name "The path of image:"))
        file-ext
        file-base)

    (setq file-ext (file-name-extension file))
    (setq file-base (file-name-sans-extension file))
    (cond
     ((member file-ext '("ttf" "eot" "woff"))
      (setq rlt (concat "@font-face {\n"
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
                        "\n}"
                        )))
     (t
      (with-temp-buffer
        (shell-command (concat "cat " file "|base64") 1)
        (setq str (replace-regexp-in-string "\n" "" (buffer-string))))
      (setq rlt (concat "background:url(\"data:image/"
                        file-ext
                        ";base64,"
                        str
                        "\") no-repeat 0 0;"
                        ))))
    (kill-new rlt)
    (copy-yank-str rlt)
    (message "css code => clipboard & yank ring")))

(defun my-css-imenu-make-index ()
  (save-excursion
    (imenu--generic-function '((nil "^ *\\([^ ]+\\) *{ *$" 1)
                               ))))


(dolist (hook '(css-mode-hook sass-mode-hook))
  (add-hook hook
            '(lambda ()
               (rainbow-mode 1)
               (unless (is-buffer-file-temp)
                 (setq imenu-create-index-function 'my-css-imenu-make-index)))))

(with-eval-after-load 'css-mode
  (define-key css-mode-map (kbd "C-c b") #'web-beautify-css))

(provide 'init-css)
