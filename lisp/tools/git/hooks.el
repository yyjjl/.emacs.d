;;; -*- lexical-binding: t; -*-

(define-hook! ymacs-git//generic-prog-mode-setup ((prog-mode-hook :append) org-mode-hook LaTeX-mode-hook)
  (when (is-buffer-suitable-for-coding!)
    (diff-hl-mode 1)))

(after! diff-hl
  (after! magit
    (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))

  (define-advice diff-hl-update (:around (-fn) async)
    (unless ymacs-git--diff-hl-update-timer
      (setq
       ymacs-git--diff-hl-update-timer
       (run-with-idle-timer
        ymacs-git--diff-hl-update-delay
        nil
        (lambda ()
          (with-demoted-errors "diff-hl error: %s"
            (funcall -fn))
          (setq ymacs-git--diff-hl-update-timer nil)))))))

(after! smerge-mode
  (define-hook! ymacs-git//maybe-enable-smerge (magit-diff-visit-file-hook)
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^<<<<<<< " nil t)
        (smerge-mode 1)
        (ymacs-hydra/smerge/body)))))
