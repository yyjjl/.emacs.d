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

;;;###autoload
(defun company/toggle-modeline ()
  (interactive)
  (let ((item '(company-mode company-lighter)))
    (if (member item mode-line-misc-info)
        (progn
          (setq mode-line-misc-info (remove item mode-line-misc-info))
          (message "Company diabled in modeline"))
      (push item mode-line-misc-info)
      (message "Company enabled in modeline"))))
