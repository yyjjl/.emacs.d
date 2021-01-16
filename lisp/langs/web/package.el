;;; -*- lexical-binding: t; -*-

(require-packages!
 emmet-mode
 typescript-mode
 web-mode)

(eval-when-has-feature! term
  (let ((repl '(:program "node" :program-args ("-l" the-file) :cmd-fmt ".load %s\n")))
    (add-to-list 'ymacs-term-repl-alist (cons 'typescript-mode repl))
    (add-to-list 'ymacs-term-repl-alist (cons 'js-mode repl))))

(eval-when-has-feature! lsp
  (ymacs-lsp//register-client 'css-ls :package 'lsp-css)
  (ymacs-lsp//register-client 'ts-ls :package 'lsp-javascript)
  (ymacs-lsp//register-client 'html-ls :package 'lsp-html))

(add-to-list
 'auto-mode-alist
 (eval-when-compile
   (cons (rx "." (or "phtml" "html" "htm" "djhtml"
                     "gsp" "jsp" "ascx" "aspx"
                     "tmpl" "tml" "ejs" "swig" "vue"
                     "php" "erb" "mustache")
             string-end)
         'web-mode)))

(setq auto-mode-alist (cl-subst 'web-mode 'js-jsx-mode auto-mode-alist))
