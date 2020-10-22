;;; -*- lexical-binding: t; -*-

(after! latex
  (define-hook! ymacs-latex|setup (LaTeX-mode-hook)
    (ymacs-latex//common-setup)

    (ymacs-lsp//try-enable latex
      :fallback
      (progn
        (company-auctex-init)

        (ymacs-company//add-backend 'company-reftex-labels :main-backend-p nil)
        (ymacs-company//add-backend 'company-reftex-citations :main-backend-p nil)
        (flycheck-mode -1)))))
