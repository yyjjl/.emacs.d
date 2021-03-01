;;; -*- lexical-binding: t; -*-

(define-hook! ymacs-web//web-setup (web-mode-hook js-mode-hook typescript-mode-hook css-mode-hook)
  (when (and (is-buffer-suitable-for-coding!)
             (not (string-suffix-p ".json" (downcase buffer-file-name))))
    (try-enable-lsp! web))

  (when (eq major-mode web-mode-hook)
    (when (equal web-mode-content-type "jsx")
      (setq-local web-mode-enable-auto-quoting nil)))

  (setq-local electric-layout-rules
              (remove (assoc ?\; electric-layout-rules) electric-layout-rules)))
