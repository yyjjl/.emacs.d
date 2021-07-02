;;; -*- lexical-binding: t; -*-

(eval-when-has-feature! lsp
  (defsubst ymacs-lsp//set-lsp-signature-width ()
    (setq lsp-signature-posframe-params
          (plist-put lsp-signature-posframe-params
                     :width (max 60 (min (/ (frame-width) 2) (window-width))))))

  (after! lsp-mode
    (define-hook! ymacs-lsp//set-lsp-signature-frame-params (lsp-signature-mode-hook)
      (setq lsp-signature-function
            (if (display-graphic-p)
                #'lsp-signature-posframe
              #'lsp-lv-message))
      (ymacs-lsp//set-lsp-signature-width))

    (setq lsp-eldoc-render-all ymacs-eldoc-use-childfeame-p)
    (setq lsp-display-inline-image t)))
