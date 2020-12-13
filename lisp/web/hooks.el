;;; -*- lexical-binding: t; -*-

(add-hook 'html-mode-hook 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'web-mode-hook 'emmet-mode)
(add-hook 'nxml-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook 'emmet-mode)

(define-hook! ymacs-web|web-setup (web-mode-hook js-mode-hook typescript-mode-hook css-mode-hook)
  ;; (electric-indent-local-mode -1)
  ;; (electric-layout-local-mode -1)
  ;; (when (eq major-mode 'css-mode)
  ;;   (rainbow-mode 1))

  (when (and (buffer-enable-rich-feature-p)
             (not (string-suffix-p ".json" (downcase buffer-file-name))))
    (ymacs-lsp//try-enable web)))
