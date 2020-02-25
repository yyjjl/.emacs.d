;; -*- lexical-binding:t -*-


(defun perl//format-region (beg end)
  (unless (char-equal ?\n (char-before end))
    (setq end (min (save-excursion ;; must including terminating newline
                     (goto-char end)
                     (1+ (line-end-position)))
                   (point-max))))
  (let ((old-point (point)))
    (apply #'call-process-region
           beg end
           perl-perltidy-path t '(t nil)
           "--quiet"
           "--standard-error-output"
           perl-perltidy-options)
    (goto-char old-point)))

;;;###autoload
(defun perl/perltidy-format (&optional -arg)
  "Format Perl code with perltidy.
If region is active, operate on it, else operate on line."
  (interactive "p")
  (if (use-region-p)
      (perl//format-region (region-beginning) (region-end))
    (cond
     ((eq -arg 1)
      (perl//format-region (save-excursion (beginning-of-defun)
                                           (point))
                           (save-excursion (end-of-defun)
                                           (point))))
     ((eq -arg 4)
      (perl//format-region (point-min) (point-max))))))


;;;###autoload
(defun run-perl (&optional -kill)
  (interactive "P")
  (let* ((buffer-name "*perl-repl*")
         (buffer (get-buffer buffer-name)))
    (unless (and buffer
                 (not -kill)
                 (process-live-p (get-buffer-process buffer))
                 (eq (buffer-local-value 'major-mode buffer)
                     'term-mode))
      (when buffer (kill-buffer buffer))
      (setq buffer (term//exec-program perl-shell-path nil buffer-name))
      (with-current-buffer buffer
        (local-set-key (kbd "C-c C-z") 'term/switch-back-no-quit)))
    (pop-to-buffer buffer)))
