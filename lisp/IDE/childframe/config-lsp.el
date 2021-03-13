;;; -*- lexical-binding: t; -*-

(eval-when-has-feature! lsp
  (after! lsp-mode
    (define-hook! ymacs-lsp//set-lsp-signature-frame-params (lsp-signature-mode-hook)
      (setq lsp-signature-function
            (if (display-graphic-p)
                #'lsp-signature-posframe
              #'lsp-lv-message))
      (ymacs-lsp//set-lsp-signature-width))


    (setq lsp-eldoc-render-all t)
    (setq lsp-display-inline-image t)))
