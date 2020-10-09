;;; -*- lexical-binding: t; -*-

(add-hook 'html-mode-hook 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'web-mode-hook 'emmet-mode)
(add-hook 'nxml-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook 'emmet-mode)

(define-hook! ymacs-web|web-setup (web-mode-hook)
  (ymacs-lsp//try-enable web
    :enable (and buffer-file-name
                 (string-match-p "\\.[jt]sx?\\'" (downcase buffer-file-name)))
    :fallback (ymacs-company//add-backend 'company-web-html))

  (electric-indent-local-mode -1))

(define-hook! ymacs-web|js-setup (js-mode-hook typescript-mode-hook)
  (ymacs-lsp//try-enable js
    :enable (and buffer-file-name
                 (not (string-suffix-p ".json" (downcase buffer-file-name)))))

  (electric-indent-local-mode -1)
  (setq-local electric-layout-rules
              (delq (assoc ?\; electric-layout-rules) electric-layout-rules)))
