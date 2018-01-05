(require-packages!
 lsp-mode
 company-lsp)



(custom-theme-set-faces
 'molokai
 '(lsp-face-highlight-textual ((t :background "#444155")))
 '(cquery-sem-free-var-face ((t :inherit default)))
 '(cquery-sem-member-var-face ((t :slant italic
                                  :inherit font-lock-variable-name-face))))

(with-eval-after-load 'lsp-mode
  (require 'lsp-flycheck)
  (require 'lsp-imenu)

  (setq lsp-enable-completion-at-point nil
        lsp-highlight-symbol-at-point nil)

  (define-key!
    ("M-s h h" . lsp-symbol-highlight)
    ("C-c r" . lsp-rename)
    ("C-c ." . lsp-info-under-point)
    ("C-c C-SPC" . lsp-execute-code-action)))

(with-eval-after-load 'company-lsp
  (setq company-lsp-async t))

(provide 'init-lsp-mode)
