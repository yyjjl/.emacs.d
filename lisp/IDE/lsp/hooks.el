;; -*- lexical-binding:t -*-

(defvar ymacs-lsp-clear-leak-timer nil)

(after! lsp-mode
  (setq ymacs-lsp-clear-leak-timer
        (run-with-timer
         30 30
         (lambda ()
           (mapc #'ymacs-lsp//clear-leak-handlers (hash-table-values lsp-clients)))))

  (define-hook! ymacs-lsp|after-open (lsp-after-open-hook)
    (remove-hook 'eldoc-documentation-functions #'flymake-eldoc-function t)
    (add-hook 'eldoc-documentation-functions #'flymake-eldoc-function -20 t)

    ;; Disable lsp eldoc to speed up, use C-c C-d to view doc
    ;; (when (lsp--capability :hoverProvider)
    ;;   (add-hook 'eldoc-documentation-functions #'ymacs-lsp//eldoc-function -10 t))

    (setq-local company-minimum-prefix-length 2))

  (define-advice lsp--calculate-root (:around (-fn -session -file-name) use-truename)
    (funcall -fn -session (file-truename -file-name)))

  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))

(after! lsp-modeline
  (setq lsp-modeline-code-actions-segments '(count name)))
