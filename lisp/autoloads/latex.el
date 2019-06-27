;;; -*- lexical-binding: t; -*-

;;;###autoload
(defun latex/narrow-to-section (&optional -no-subsections)
  (interactive "P")
  (save-mark-and-excursion
    (LaTeX-mark-section -no-subsections)
    (call-interactively 'narrow-to-region)))

;;;###autoload
(defun latex/count-words ()
  (interactive)
  (if (region-active-p)
      (call-interactively 'tex-count-words)
    (let* ((options (or (and (eq current-prefix-arg '(16))
                             (read-string "Options: " "-inc -ch -brief"))
                        "-inc -ch -brief"))
           (file (or (cond ((not current-prefix-arg)
                            (expand-file-name (TeX-master-file "tex")))
                           ((equal current-prefix-arg '(4))
                            (buffer-file-name)))
                     (read-file-name "TeX file: "))))
      (let ((default-directory (TeX-master-directory)))
        (with-current-buffer (compilation-start (concat "texcount " options " "
                                                        (shell-quote-argument file)))
          (add-transient-hook! (compilation-finish-functions
                                :local t
                                :name latex|after-count-words
                                :arguments (buffer _))
            (with-current-buffer buffer
              (goto-char (point-min))
              (re-search-forward "^texcount" nil :noerror)
              (forward-line 0))))))))

;;;###autoload
(defun latex/build ()
  (interactive)
  (reftex-parse-all)
  (let ((TeX-save-query nil))
    (TeX-save-document (TeX-master-file)))
  (let ((command (if (save-excursion
                       (goto-char 1)
                       (search-forward-regexp
                        "\\\\usepackage\\s-*{\\s-*minted"
                        nil t))
                     "XeLaTeX"
                   TeX-command-default)))
    (TeX-command command 'TeX-master-file -1)))

;;;###autoload
(defun latex/skip-close-pair ()
  (interactive)
  (let ((char (char-after)))
    (if (and (equal char (string-to-char (this-command-keys)))
             (member char '(?\) ?\} ?\])))
        (forward-char)
      (self-insert-command 1))))

;;;###autoload
(defun latex/force-update-style ()
  (interactive)
  (TeX-update-style t))

;;;###autoload
(defun latex/forward-sexp (&optional -n)
  (interactive "^p")
  (let ((current-point (point)))
    (if (save-excursion
          (LaTeX-find-matching-begin)
          (let ((target-point (point)))
            (and (looking-at "\\\\begin{[a-zA-Z0-9\\*]+}")
                 (< current-point (match-end 0))
                 (>= current-point target-point))))
        (LaTeX-find-matching-end)
      (forward-sentence-or-sexp -n))))

;;;###autoload
(defun latex/backward-sexp (&optional -n)
  (interactive "^p")
  (let ((current-point (point)))
    (if (save-excursion
          (LaTeX-find-matching-end)
          (let ((target-point (point)))
            (and (looking-back "\\\\end{[a-zA-Z0-9\\*]+}" (line-beginning-position))
                 (< current-point target-point)
                 (>= current-point (match-beginning 0)))))
        (LaTeX-find-matching-begin)
      (backward-sentence-or-sexp -n))))
