;;; -*- lexical-binding: t; -*-

(add-hook 'dired-mode-hook #'diff-hl-dired-mode)

(define-hook! ymacs-git|generic-prog-mode-setup ((prog-mode-hook :append) org-mode-hook LaTeX-mode-hook)
  (when (buffer-enable-rich-feature-p)
    (diff-hl-mode 1)))

(after! git-messenger
  (define-hook! (kill-commit-id msg) (git-messenger:after-popup-hook)
    ;; extract commit id and put into the kill ring
    (when (string-match "\\(commit *: *\\)\\([0-9a-z]+\\)" msg)
      (kill-new (match-string 2 msg))
      (message "commit hash %s => kill-ring" (match-string 2 msg)))))
