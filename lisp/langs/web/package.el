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

(add-auto-mode! 'web-mode
  "\\.phtml\\'" "\\.cmp\\'" "\\.app\\'"
  "\\.page\\'" "\\.component\\'"
  "\\.wp\\'" "\\.tmpl\\'" "\\.php\\'"
  "\\.module\\'" "\\.inc\\'" "\\.hbs\\'"
  "\\.tpl\\'" "\\.[gj]sp\\'" "\\.as[cp]x\\'"
  "\\.erb\\'" "\\.mustache\\'"
  "\\.djhtml\\'" "\\.ftl\\'"
  "\\.html?\\'" "\\.xul?\\'" "\\.eex?\\'")

(setq auto-mode-alist (cl-subst 'web-mode 'js-jsx-mode auto-mode-alist))
