;;; -*- lexical-binding: t; -*-

(defvar TeX-save-query)
(declare-function TeX-active-buffer "tex-buf")
(declare-function TeX-next-error "tex-buf")
(declare-function TeX-previous-error "tex-buf")
(declare-function reftex-parse-all "reftex-parse")

;;;###autoload
(defun ymacs-latex/next-error ()
  (interactive)
  (call-interactively
   (let ((buffer (ymacs-default//get-error-buffer)))
     (if (and (buffer-live-p buffer)
              (window-live-p (get-buffer-window buffer)))
         #'next-error
       #'TeX-next-error))))

;;;###autoload
(defun ymacs-latex/previous-error ()
  (interactive)
  (call-interactively
   (let ((buffer (ymacs-default//get-error-buffer)))
     (if (and (buffer-live-p buffer)
              (window-live-p (get-buffer-window buffer)))
         #'previous-error
       #'TeX-previous-error))))


;;;###autoload
(defun ymacs-latex/narrow-to-section (&optional -no-subsections)
  (interactive "P")
  (save-mark-and-excursion
    (LaTeX-mark-section -no-subsections)
    (call-interactively 'narrow-to-region)))

;;;###autoload
(defun ymacs-latex/count-words ()
  "
If call when region is active, then call `tex-count-words'.
If call with prefix C-u, then run on current file.
If call with prefix C-u C-u, then prompt to select a master file.
Else call with `read-file-name'.
"
  (interactive)
  (if (region-active-p)
      (call-interactively 'tex-count-words)
    (let* ((options
            (or (and (eq current-prefix-arg '(16))
                     (read-string "Options: " "-inc -ch -brief"))
                "-inc -ch -brief"))
           (file
            (or (cond ((not current-prefix-arg)
                       (expand-file-name (TeX-master-file "tex")))
                      ((equal current-prefix-arg '(4))
                       (buffer-file-name)))
                (read-file-name "TeX file: ")))
           (default-directory (TeX-master-directory))
           (command (format "texcount %s %s" options (shell-quote-argument file))))
      (run-compilation!
       :-command command
       :-callback (lambda (&rest _) (goto-char (point-min)))))))

;;;###autoload
(defun ymacs-latex/build ()
  (interactive)
  (reftex-parse-all)
  (TeX-save-document (TeX-master-file))
  (TeX-command (cond (ymacs-latexmk-path "LatexMk")
                     ((eq TeX-engine 'xetex) "XeLaTeX")
                     (t "LaTeX"))
               'TeX-master-file -1))

;;;###autoload
(defun ymacs-latex/force-update-style ()
  (interactive)
  (TeX-update-style t))

;;;###autoload
(defun ymacs-latex/forward-sexp (&optional -n)
  (interactive "^p")
  (let ((current-point (point)))
    (if (save-excursion
          (LaTeX-find-matching-begin)
          (let ((target-point (point)))
            (and (looking-at "\\\\begin{[a-zA-Z0-9\\*]+}")
                 (< current-point (match-end 0))
                 (>= current-point target-point))))
        (LaTeX-find-matching-end)
      (ymacs-editor/forward-sentence-or-sexp -n))))

;;;###autoload
(defun ymacs-latex/backward-sexp (&optional -n)
  (interactive "^p")
  (let ((current-point (point)))
    (if (save-excursion
          (LaTeX-find-matching-end)
          (let ((target-point (point)))
            (and (looking-back "\\\\end{[a-zA-Z0-9\\*]+}" (line-beginning-position))
                 (< current-point target-point)
                 (>= current-point (match-beginning 0)))))
        (LaTeX-find-matching-begin)
      (ymacs-editor/backward-sentence-or-sexp -n))))
