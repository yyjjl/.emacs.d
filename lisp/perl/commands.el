;; -*- lexical-binding:t -*-

(defun ymacs-perl//format-region (beg end)
  (unless (char-equal ?\n (char-before end))
    (setq end (min (save-excursion ;; must including terminating newline
                     (goto-char end)
                     (1+ (line-end-position)))
                   (point-max))))
  (let ((old-point (point)))
    (apply #'call-process-region
           beg end
           ymacs-perl-perltidy-path t '(t nil)
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
      (ymacs-perl//format-region (save-excursion (beginning-of-defun)
                                                 (point))
                                 (save-excursion (end-of-defun)
                                                 (point))))
     ((eq -arg 4)
      (ymacs-perl//format-region (point-min) (point-max))))))

;;;###autoload
(defun ymacs-perl/repl (&optional _kill)
  (interactive "P")
  (ymacs-term//exec-program-reuse-buffer
   "perl-repl"
   ymacs-perl-shell-path nil
   :callback (lambda () (local-set-key (kbd "C-c C-z") 'ymacs-term/switch-back-no-quit))))
