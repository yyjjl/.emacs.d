;;; -*- lexical-binding: t; -*-

;;;###autoload
(defun ymacs-editor/complete-common ()
  (interactive)
  (unless (ignore-errors (yas-expand))
    (company-complete-common)))

;;;###autoload
(defun ymacs-editor/toggle-company-ispell ()
  "Toggle `company-ispell'"
  (interactive)
  (cond
   ((memq 'company-ispell company-backends)
    (setq company-backends (delete 'company-ispell company-backends))
    (message "company-ispell disabled"))
   (t
    ;; Company-ispell it will stop other completions
    (add-to-list 'company-backends 'company-ispell)
    (message "company-ispell enabled!"))))
