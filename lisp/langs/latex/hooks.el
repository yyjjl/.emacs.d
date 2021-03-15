;;; -*- lexical-binding: t; -*-

(declare-function reftex-toc-rescan "reftex")

(defun ymacs-latex//build-on-save (&optional -do-build)
  (when (or -do-build
            ymacs-latex-build-on-save-p)
    (let* ((buffer (TeX-process-buffer (TeX-master-file)))
           (proc (when (buffer-live-p buffer)
                   (get-buffer-process buffer))))
      (if (and (not -do-build)
               (process-live-p proc))
          (message "Compilation process is running ...")
        (TeX-command (cond (ymacs-latexmk-path "LatexMk")
                           ((eq TeX-engine 'xetex) "XeLaTeX")
                           (t "LaTeX"))
                     'TeX-master-file -1)))))

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

    (hs-minor-mode 1)
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

    (flymake-mode 1)
    ;; only run flymake after saving current buffer
    (remove-hook ' after-change-functions #'flymake-after-change-function t)

    (add-hook 'after-save-hook #'ymacs-latex//build-on-save nil t)

    (setq-local tab-width 2)

    (eval-when-has-feature! lsp
      (ymacs-lsp//try-enable-simple latex))))
