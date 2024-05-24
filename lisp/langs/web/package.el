;;; -*- lexical-binding: t; -*-

(require-packages!
 typescript-mode)

(eval-when-has-feature! term
  (let ((repl '(:program "node" :program-args () :cmd-fmt ".load %s\n")))
    (dolist (mode '(typescript-mode typescript-ts-mode js-mode js-ts-mode))
      (add-to-list 'ymacs-term-repl-alist (cons mode repl)))))

(eval-when-has-feature! lsp
  (ymacs-lsp//register-client 'css-ls :package 'lsp-css)
  (ymacs-lsp//register-client 'ts-ls :package 'lsp-javascript)
  (ymacs-lsp//register-client 'html-ls :package 'lsp-html))
