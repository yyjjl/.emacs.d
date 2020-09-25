;;; -*- lexical-binding: t; -*-

(add-hook 'html-mode-hook 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'web-mode-hook 'emmet-mode)
(add-hook 'nxml-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook 'emmet-mode)

(define-hook! ymacs-web|web-setup (web-mode-hook)
  (if (and buffer-file-name
           (string-match-p "\\.[jt]sx?\\'" (downcase buffer-file-name)))
      (add-transient-hook!
          (hack-local-variables-hook :local t :name ymacs-web|web-internal)
        (ymacs-web|common-setup)))
  (ymacs-company//add-backend 'company-web-html))

(define-hook! ymacs-web|js-setup (js-mode-hook typescript-mode-hook)
  (when (and buffer-file-namex
             (not (string-suffix-p ".json" (downcase buffer-file-name))))
    (add-transient-hook!
        (hack-local-variables-hook :local t :name ymacs-web|js-internal)
      (web|setup-internal)))

  (setq-local electric-layout-rules
              (delq (assoc ?\; electric-layout-rules) electric-layout-rules)))
