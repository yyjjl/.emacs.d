;; -*- lexical-binding:t -*-

(defun ymacs-perl//library-search-paths (-perl-executable)
  (--> -perl-executable
       (format "%s -e 'for(@INC){print \"$_\n\";}'" it)
       (shell-command-to-string it)
       (split-string it "\n" :omit-nulls "\n\t ")
       (cl-remove-if-not #'file-exists-p it)))

(defun ymacs-perl//format-region (-beg -end)
  (unless (and ymacs-perltidy-path
               (file-executable-p ymacs-perltidy-path))
    (user-error "perltidy can not be found"))

  (unless (char-equal ?\n (char-before -end))
    (setq -end (min (save-excursion ;; must including terminating newline
                      (goto-char -end)
                      (1+ (line-end-position)))
                    (point-max))))
  (let ((old-point (point)))
    (apply #'call-process-region
           -beg -end
           ymacs-perltidy-path t '(t nil)
           "--quiet"
           "--standard-error-output"
           ymacs-perl-perltidy-options)
    (goto-char old-point)))

;;;###autoload
(defun ymacs-perl/perltidy-format (&optional -arg)
  "Format Perl code with perltidy.
If region is active, operate on it, else operate on line."
  (interactive "p")
  (if (use-region-p)
      (ymacs-perl//format-region (region-beginning) (region-end))
    (cond
     ((eq -arg 1)
      (ymacs-perl//format-region (save-excursion (beginning-of-defun) (point))
                                 (save-excursion (end-of-defun) (point))))
     ((eq -arg 4)
      (ymacs-perl//format-region (point-min) (point-max))))))
