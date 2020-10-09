;;; -*- lexical-binding: t; -*-

(require-packages!
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
  "\\.html?\\'" "\\.xul?\\'" "\\.eex?\\'")

(setq auto-mode-alist (cl-subst 'web-mode 'js-jsx-mode auto-mode-alist))

(provide 'init-web)
