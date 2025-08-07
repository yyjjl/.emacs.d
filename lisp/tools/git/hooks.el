;;; -*- lexical-binding: t; -*-

(define-hook! ymacs-git//generic-prog-mode-setup ((prog-mode-hook :append) org-mode-hook LaTeX-mode-hook)
  (when (is-buffer-suitable-for-coding!)
    (diff-hl-mode 1)))

(after! diff-hl
  (after! magit
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)))

(after! smerge-mode
  (define-hook! ymacs-git//maybe-enable-smerge (magit-diff-visit-file-hook)
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^<<<<<<< " nil t)
        (smerge-mode 1)
        (message "Run \\[ymacs-transient/smerge] to invoke smerge command panel")
        ;; (transient-setup 'ymacs-transient/smerge)
        ))))
