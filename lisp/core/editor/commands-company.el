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

;;;###autoload
(defun ymacs-editor/company-number ()
  "Forward to `company-complete-number'.
Unless the number is potentially part of the candidate.
In that case, insert the number.
@see https://github.com/redguardtoo/emacs.d/blob/master/lisp/init-company.el"
  (interactive)
  (let* ((key (this-command-keys))
         (prefix (concat company-prefix key))
         (n (if (equal key "0") 10 (string-to-number key))))
    (cond
     ((and (eq n 0))
      (company-filter-candidates))

     ((or (cl-find-if (lambda (-str) (string-prefix-p prefix -str))
                      company-candidates)
          (> n (length company-candidates))
          (looking-back "[0-9]+\\.[0-9]*" (line-beginning-position)))
      (self-insert-command 1))

     (t
      (company-complete-number n)))))
