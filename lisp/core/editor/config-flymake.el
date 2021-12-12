;;; -*- lexical-binding: t; -*-

(after! flymake
  (define-hook! ymacs-editor//setup-flymake (flymake-mode-hook)
    (when flymake-mode
      (remove-hook 'flymake-diagnostic-functions #'flymake-cc t)
      (unless next-error-function
        (setq-local next-error-function #'flymake-goto-next-error))))

  (define-advice flymake-goto-next-error (:around (-fn &optional -n -filter -interactive))
    (let ((min-severity (flymake--severity :warning)))
      (when (cl-some
             (lambda (ov)
               (when-let (diag (overlay-get ov 'flymake-diagnostic))
                 (>= (flymake--severity (flymake--diag-type diag)) min-severity)))
             (flymake--overlays))
        (setq -filter '(:error :warning)))
      (funcall -fn -n -filter -interactive)))

  (define-advice flymake-diag-region (:override (-buffer -line &optional -col) fast)
    (condition-case-unless-debug _err
        (with-current-buffer -buffer
          (let ((-line (max -line 1)))
            (save-excursion
              (save-match-data
                (goto-char (point-min))
                (forward-line (1- -line))
                (when (and -col (cl-plusp -col))
                  (forward-char (1- -col)))
                (let ((beg (point)))
                  (cons beg
                        (min (point-max)
                             (max (1+ beg)
                                  (or (ignore-errors (end-of-thing 'symbol)) beg)))))))))
      (error
       (flymake-log :warning "Invalid region line=%s col=%s" -line -col)
       nil)))

  (define-key! :map flymake-mode-map
    ("C-c f l" . flymake-show-buffer-diagnostics))

  (define-key! :map flymake-diagnostics-buffer-mode-map
    ("n" . next-line)
    ("j" . next-line)
    ("p" . previous-line)
    ("k" . previous-line))

  (setq-default flymake-diagnostic-functions nil))
