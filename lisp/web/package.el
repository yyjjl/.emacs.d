;;; -*- lexical-binding: t; -*-

(require-packages!
 lsp-mode
 emmet-mode
 web-mode)

(ymacs-lsp//register-client 'css-ls :package 'lsp-css)
(ymacs-lsp//register-client 'ts-ls :package 'lsp-javascript)
(ymacs-lsp//register-client 'html-ls :package 'lsp-html)

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

(provide 'init-web)
