;;; -*- lexical-binding: t; -*-

(declare-function reftex-toc-rescan "reftex")

(after! reftex
  (add-hook 'reftex-toc-mode-hook #'reftex-toc-rescan)
  (define-hook! ymacs-latex//reftex-select-bib-setup (reftex-select-bib-mode-hook)
    (setq truncate-lines nil)))

(after! tex
  (define-advice TeX-font (:around (-fn -replace -what) no-modifier)
    ;; auto add control modifier
    (funcall -fn -replace (event-convert-list (list 'control -what)))))

(after! latex
  (define-hook! ymacs-latex//setup (LaTeX-mode-hook)
    (prettify-symbols-mode 1)

    (unless TeX-master
      (setq TeX-master 'dwim))

    (reftex-mode 1)
    ;; conflict with latex-mode
    (electric-pair-local-mode -1)
    (electric-indent-local-mode -1)
    (LaTeX-math-mode 1)

    (TeX-fold-mode 1)
    ;; (TeX-interactive-mode 1)
    (TeX-source-correlate-mode 1)
    (TeX-PDF-mode 1)

    (outline-minor-mode 1)

    (flycheck-mode 1)

    (setq-local tab-width 2)
    (when (is-buffer-suitable-for-coding!)
      (try-enable-lsp! latex))))
