;;; -*- lexical-binding: t; -*-

(defun ymacs-tabnine//comapny-transformer (-candidates)
  (let ((lst -candidates)
        candidate)
    (while (and (setq candidate (cadr lst))
                (not (eq (get-text-property 0 'company-backend candidate) 'company-tabnine)))
      (setq lst (cdr lst)))
    (when candidate
      (setcdr lst (cddr lst)) ;; remove candidate
      (setcdr -candidates (cons candidate (cdr -candidates))))
    -candidates))

(defsubst ymacs-tabnine//check ()
  (unless (and (require 'company-tabnine nil t)
               (company-tabnine--executable-path))
    (user-error "Can not load TabNine")))

(defsubst ymacs-tabnine//enable-p ()
  (memq 'company-tabnine
        (car (ymacs-editor//find-main-company-backend company-backends))))

(defun ymacs-tabnine/disable ()
  (interactive)
  (when (ymacs-tabnine//enable-p)
    (remove-hook 'company-transformers #'ymacs-tabnine//comapny-transformer t)

    (let ((parent-of-main-backend (ymacs-editor//find-main-company-backend company-backends)))
      (setcar parent-of-main-backend (remove 'company-tabnine (car parent-of-main-backend)))))

  (when (called-interactively-p 'interactive)
    (message "TabNine is disabled")))

(defun ymacs-tabnine/enable ()
  (interactive)
  (ymacs-tabnine//check)

  (unless (ymacs-tabnine//enable-p)
    (remove-hook 'company-transformers #'ymacs-tabnine//comapny-transformer t)
    (setq-local company-transformers (append company-transformers '(ymacs-tabnine//comapny-transformer)))

    (let ((parent-of-main-backend (ymacs-editor//find-main-company-backend company-backends)))
      (insert-before! :with 'company-tabnine (car parent-of-main-backend))))

  (when (called-interactively-p 'interactive)
    (message "TabNine is enabled")))
