(require-packages!
 tide
 js-doc)




;; (with-eval-after-load 'js
;;   (require 'lsp-javascript-typescript)
;;   (define-key js-mode-map (kbd "C-c C-c") 'js/load-run))

(with-eval-after-load 'tide
  (define-key! :map tide-mode-map
    ("C-c C-d" . tide-documentation-at-point)
    ("M-?" . tide-references)
    ("C-c r r" . tide-refactor)
    ("C-c r n" . tide-rename-symbol)
    ("C-c r f" . tide-rename-file)
    ("C-c b" . tide-format)))

(define-hook! js|setup (js-mode-hook typescript-mode-hook)
  (unless (and buffer-file-name
               (string-suffix-p ".json" (downcase buffer-file-name)))
    (tide-setup)

    (setq-local flycheck-check-syntax-automatically '(save mode-enabled))

    (add-to-list 'company-backends 'company-tide)

    (tide-hl-identifier-mode 1))

  (setq-local electric-layout-rules
              (delq (assoc ?\; electric-layout-rules)
                    electric-layout-rules)))

(provide 'init-javascript)
