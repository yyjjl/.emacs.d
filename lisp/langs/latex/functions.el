;;; -*- lexical-binding: t; -*-

;; copy from https://github.com/tom-tan/auctex-latexmk
(defun ymacs-latex//latexmk-sentinel (process name)
  (save-excursion
    (goto-char (point-max))
    (cond
     ((re-search-backward (format "^%s finished at" mode-name) nil t)
      (if (re-search-backward "^Run number [0-9]+ of rule '\\(pdf\\|lua\\|xe\\)?latex'" nil t)
          (progn
            (forward-line 5)
            (let ((beg (point)))
              (when (string= (current-word) "Latexmk")
                ;; Special treatment for MiKTeX
                (forward-line))
              (re-search-forward "^Latexmk:" nil t)
              (beginning-of-line)
              (save-restriction
                (narrow-to-region beg (point))
                (goto-char (point-min))
                (TeX-LaTeX-sentinel process name))))
        (message (format "%s: nothing to do" name))))
     ((re-search-backward (format "^%s exited abnormally with code" mode-name) nil t)
      (re-search-backward "^Collected error summary (may duplicate other messages):" nil t)
      (re-search-forward "^  \\([^:]+\\):" nil t)
      (let ((com (TeX-match-buffer 1)))
        (cond
         ((string-match "^\\(pdf\\|lua\\|xe\\)?latex" com)
          (goto-char (point-min))
          (TeX-LaTeX-sentinel process name)
          (when (string= TeX-command-next TeX-command-BibTeX)
            (setq TeX-command-default nil)))
         ((string-match "^bibtex " com)
          (forward-line -1)
          (re-search-backward com nil t)
          (forward-line 5)
          (let ((beg (point)))
            (re-search-forward "^Rule" nil t)
            (beginning-of-line)
            (save-restriction
              (narrow-to-region beg (point))
              (TeX-BibTeX-sentinel process name))))))))))

(defun TeX-run-latexmk (name command file)
  (let ((TeX-sentinel-default-function 'ymacs-latex//latexmk-sentinel))
    (TeX-run-TeX name command file)))

(defun ymacs-latex//setup-latexmk ()
  "Add LatexMk command to TeX-command-list."
  (require 'tex)
  (require 'latex)

  (add-to-list 'TeX-expand-list
               '("%(-PDF)"
                 (lambda ()
                   (cond
                    ((and (eq TeX-engine 'default) TeX-PDF-mode) "-pdf ")
                    ((and (eq TeX-engine 'xetex) TeX-PDF-mode) "-pdf -pdflatex=xelatex ")
                    ((eq TeX-engine 'xetex) "-xelatex ")
                    ((eq TeX-engine 'luatex) "-lualatex ")
                    (t "")))))

  (add-to-list 'TeX-command-list
               '("LaTeXMK"
                 "latexmk %(-PDF)%S%(mode) %(file-line-error) %(extraopts) %t"
                 TeX-run-latexmk
                 nil
                 (plain-tex-mode latex-mode doctex-mode)
                 :help "Run LaTeXMK"))

  (setq-default LaTeX-clean-intermediate-suffixes
                (append LaTeX-clean-intermediate-suffixes '("\\.fdb_latexmk" "\\.aux.bak" "\\.fls"))))
