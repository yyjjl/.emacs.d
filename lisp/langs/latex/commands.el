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
   (let ((buffer (ymacs-editor//get-error-buffer))
         (help-buffer (get-buffer "*TeX Help*")))
     (if (or (not (and (buffer-live-p help-buffer)
                       (window-live-p (get-buffer-window help-buffer))))
             (and
              (buffer-live-p buffer)
              (window-live-p (get-buffer-window buffer))))
         #'next-error
       #'TeX-next-error))))

;;;###autoload
(defun ymacs-latex/previous-error ()
  (interactive)
  (call-interactively
   (let ((buffer (ymacs-editor//get-error-buffer))
         (help-buffer (get-buffer "*TeX Help*")))
     (if (or (not (and (buffer-live-p help-buffer)
                       (window-live-p (get-buffer-window help-buffer))))
             (and
              (buffer-live-p buffer)
              (window-live-p (get-buffer-window buffer))))
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
            (or (and (equal current-prefix-arg '(16))
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
      (run-compilation! :command command))))

;;;###autoload
(defun ymacs-latex/build ()
  (interactive)
  (reftex-parse-all)
  (TeX-save-document (TeX-master-file))
  (ymacs-latex//build-on-save :do-build))

;;;###autoload
(defun ymacs-latex/force-update-style ()
  (interactive)
  (TeX-update-style t))

;;;###autoload
(defun ymacs-latex/forward-sexp (&optional -n)
  (interactive "^p")
  (let* ((line-beg (line-beginning-position))
         (line-end (line-end-position))
         (current-point (point))
         (env-beg (ignore-errors
                    (save-excursion
                      (skip-chars-forward " \t" line-end)
                      (when (eq (char-after) ?\\)
                        (forward-char 1))
                      (LaTeX-find-matching-begin)
                      (forward-char 7)
                      (point))))
         (env-end (ignore-errors
                    (save-excursion
                      (skip-chars-forward " \t" line-end)
                      (when (eq (char-after) ?\\)
                        (forward-char 1))
                      (LaTeX-find-matching-end)
                      (search-backward "\\end{" (line-beginning-position))
                      (point)))))
    (cond
     ((and (integer-or-marker-p env-beg)
           (> env-beg current-point)
           (< env-beg line-end))
      (if (>= -n 0)
          (goto-char env-beg)
        (forward-sentence -n)))
     ((and (integer-or-marker-p env-beg)
           (>= env-beg line-beg)
           (<= env-beg current-point))
      (if (>= -n 0)
          (goto-char env-end)
        (goto-char env-beg)
        (when (= env-beg current-point)
          (search-backward "\\begin{" line-beg t))))
     ((and (integer-or-marker-p env-end)
           (> env-end current-point)
           (< env-end line-end))
      (if (>= -n 0)
          (goto-char env-end)
        (forward-sentence -n)))
     ((and (integer-or-marker-p env-end)
           (>= env-end line-beg)
           (<= env-end current-point))
      (if (>= -n 0)
          (if (= env-end current-point)
              ;; skip to point after \end{
              (forward-char 5)
            (forward-sentence -n))
        (if (= env-end current-point)
            (goto-char env-beg)
          (goto-char env-end))))
     (t (forward-sentence -n)))))

;;;###autoload
(defun ymacs-latex/backward-sexp (&optional -n)
  (interactive "^p")
  (ymacs-latex/forward-sexp (- -n)))
