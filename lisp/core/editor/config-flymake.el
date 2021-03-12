;;; -*- lexical-binding: t; -*-

(after! flymake
  (define-hook! ymacs-editor//setup-flymake (flymake-mode-hook)
    (when flymake-mode
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

  (define-key! :map flymake-mode-map
    ("C-c f l" . flymake-show-diagnostics-buffer))

  (define-key! :map flymake-diagnostics-buffer-mode-map
    ("n" . next-line)
    ("j" . next-line)
    ("p" . previous-line)
    ("k" . previous-line))

  (setq-default flymake-diagnostic-functions nil))
