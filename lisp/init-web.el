;;; -*- lexical-binding: t; -*-

(require-packages!
 tide
 emmet-mode
 web-mode
 company-web
 rainbow-mode
 css-eldoc)

(add-auto-mode! 'web-mode
  "\\.phtml\\'" "\\.cmp\\'" "\\.app\\'"
  "\\.page\\'" "\\.component\\'"
  "\\.wp\\'" "\\.tmpl\\'" "\\.php\\'"
  "\\.module\\'" "\\.inc\\'" "\\.hbs\\'"
  "\\.tpl\\'" "\\.[gj]sp\\'" "\\.as[cp]x\\'"
  "\\.erb\\'" "\\.mustache\\'"
  "\\.djhtml\\'" "\\.ftl\\'"
  "\\.html?\\'" "\\.xul?\\'" "\\.eex?\\'"
  "\\.xml?\\'")

(setq auto-mode-alist
      (cl-subst 'web-mode 'js-jsx-mode
                (cl-subst 'web-mode 'javascript-mode auto-mode-alist)))



(defun css//font-file-to-base64 (-file)
  (if (file-exists-p -file)
      (replace-regexp-in-string "\n" "" (shell-command-to-string
                                         (concat "cat " -file "|base64")))
    ""))

;; Colourise CSS colour literals
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

(defun css//imenu-make-index ()
  (save-excursion
    (imenu--generic-function '((nil "^ *\\([^ ]+\\) *{ *$" 1)))))

(define-hook! web|setup-web (web-mode-hook)
  (if (and buffer-file-name
           (string-match-p "\\.[jt]s[x]?\\'" (downcase buffer-file-name)))
      (add-transient-hook!
          (hack-local-variables-hook :local t :name web|setup-js-internal)
        (setq-local web-mode-enable-auto-quoting nil)
        (when (bound-and-true-p lsp-enable-in-project-p)
          (electric-indent-local-mode -1)
          (setq-local flycheck-check-syntax-automatically '(save mode-enabled))
          (flycheck-mode 1)
          (tide-setup)
          (tide-hl-identifier-mode 1)
          (add-to-list 'company-backends 'company-tide))))
  (add-to-list 'company-backends 'company-web-html))

(define-hook! web|setup-js (js-mode-hook typescript-mode-hook)
  (when (and buffer-file-name
             (not (string-suffix-p ".json" (downcase buffer-file-name))))
    (add-transient-hook!
        (hack-local-variables-hook :local t :name web|setup-js-internal)
      (when (bound-and-true-p lsp-enable-in-project-p)
        (electric-indent-local-mode -1)
        (setq-local flycheck-check-syntax-automatically '(save mode-enabled))
        (flycheck-mode 1)
        (tide-setup)
        (add-to-list 'company-backends 'company-tide)
        (tide-hl-identifier-mode 1))))

  (setq-local electric-layout-rules
              (delq (assoc ?\; electric-layout-rules)
                    electric-layout-rules)))

(autoload 'turn-on-css-eldoc "css-eldoc")
(define-hook! web|setup-css (css-mode-hook sass-mode-hook)
  (rainbow-mode 1)
  (turn-on-css-eldoc)

  (setq imenu-create-index-function 'css//imenu-make-index))

(with-eval-after-load 'tide
  (define-key! :map tide-mode-map
    ("C-c C-d" . tide-documentation-at-point)
    ("M-?" . tide-references)
    ("C-c r r" . tide-refactor)
    ("C-c r n" . tide-rename-symbol)
    ("C-c r f" . tide-rename-file)
    ("C-c b" . tide-format)
    ("C-c B" . tide-organize-imports)
    ("C-c C-b" . tide-format)))

(with-eval-after-load 'web-mode
  (setq web-mode-enable-auto-closing t) ;; Enable auto close tag in `web-mode'
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-css-colorization t)
  ;; Auto expand s/ => <span>|</span>
  (setq web-mode-enable-auto-expanding t)
  (setq web-mode-enable-comment-interpolation t)
  (setq web-mode-enable-element-tag-fontification t)
  (setq web-mode-imenu-regexp-list
        '(("<\\(h[1-9]\\)\\([^>]*\\)>\\([^<]*\\)" 1 3 ">" nil)
          ("^[ \t]*<\\([@a-z]+\\)[^>]*>? *$" 1
           " id=\"\\([a-zA-Z0-9_]+\\)\"" "#" ">")
          ("^[ \t]*<\\(@[a-z.]+\\)[^>]*>? *$" 1
           " contentId=\"\\([a-zA-Z0-9_]+\\)\"" "=" ">")
          ;; angular imenu
          (" \\(ng-[a-z]*\\)=\"\\([^\"]+\\)" 1 2 "=")))

  (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))
  (setq web-mode-enable-auto-indentation nil)

  (remap! "C-c C-a" "C-c a" web-mode-map)
  (remap! "C-c C-d" "C-c d" web-mode-map)
  (remap! "C-c C-e" "C-c e" web-mode-map)
  (remap! "C-c C-t" "C-c t" web-mode-map)

  (define-key! :map web-mode-map
    ("M-e" . web-mode-element-next)
    ("M-a" . web-mode-element-previous)))


(with-eval-after-load 'projectile
  (projectile-register-project-type 'npm '("package.json")
                                    :configure "npm install"
                                    :compile "npm run build"
                                    :test "npm test"
                                    :run "npm start"
                                    :test-suffix ".test"))

(add-hook 'html-mode-hook 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'web-mode-hook 'emmet-mode)
(add-hook 'nxml-mode 'emmet-mode)
(add-hook 'css-mode-hook  'emmet-mode)

(provide 'init-web)
