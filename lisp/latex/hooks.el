;;; -*- lexical-binding: t; -*-

(after! latex
  (define-hook! ymacs-latex|setup (LaTeX-mode-hook)
    (flycheck-mode -1)

    (company-auctex-init)

    (ymacs-company//add-backend 'company-reftex-labels :main-backend-p nil)
    (ymacs-company//add-backend 'company-reftex-citations :main-backend-p nil)

    (setq next-error-function 'TeX-next-error)

    (setq-local prettify-symbols-alist tex--prettify-symbols-alist)

    (add-function
     :override (local 'prettify-symbols-compose-predicate)
     #'tex--prettify-symbols-compose-p)

    (prettify-symbols-mode 1)

    (unless TeX-master
      (setq TeX-master 'dwim))

    ;; Will conflict with latex-mode
    (electric-pair-local-mode -1)
    (electric-indent-local-mode -1)
    (TeX-fold-mode 1)
    (turn-off-auto-fill)
    (LaTeX-math-mode 1)

    (when (buffer-enable-rich-feature-p)
      (reftex-mode 1)
      (TeX-source-correlate-mode 1)
      (TeX-PDF-mode 1)

      (setq TeX-engine 'xetex)

      (outline-minor-mode 1))))
