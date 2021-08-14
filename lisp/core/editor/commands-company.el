;;; -*- lexical-binding: t; -*-

;;;###autoload
(defun ymacs-editor/complete-common ()
  (interactive)
  (unless (ignore-errors (yas-expand))
    (company-complete-common)))
