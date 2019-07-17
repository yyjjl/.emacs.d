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
(defun company/toggle-tabnine ()
  (interactive)
  (unless (and (require 'company-tabnine nil t)
               (company-tabnine--executable-path))
    (error "Can not load TabNine"))

  (let ((backend (car company-backends))
        new-backend
        message)
    (if (and (listp backend)
             (memq 'company-tabnine backend))
        (progn
          (setq message "disabled")
          (setq new-backend (remove 'company-tabnine backend))
          (when (eq (car (last new-backend)) :with)
            (setq new-backend (nbutlast new-backend 1)))
          (when (null (cdr new-backend))
            (setq new-backend (car new-backend)))

          (setq company-transformers
                (remove 'company-sort-by-backend-importance company-transformers)))
      (setq message "enabled")
      (setq new-backend
            (if (listp backend)
                (append backend
                        (if (memq :with backend) nil '(:with))
                        '(company-tabnine))
              (list backend :with 'company-tabnine)))
      (add-to-list 'company-transformers 'company-sort-by-backend-importance :append))

    (setcar company-backends new-backend)
    (message "TabNine %s: %s => %s" message backend new-backend)))
