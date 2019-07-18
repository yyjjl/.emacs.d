;;; -*- lexical-binding: t; -*-

;;;###autoload
(defun core/toggle-company-ispell ()
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

;;;###autoload
(defun core/company-yasnippet ()
  "Call `company-yasnippet'"
  (interactive)
  (company-abort)
  (or (ignore-errors (call-interactively 'company-yasnippet))
      (call-interactively 'company-abbrev)))
