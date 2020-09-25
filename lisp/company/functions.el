;;; -*- lexical-binding: t; -*-

(defsubst ymacs-company//find-main-backend (backends)
  (let ((x backends))
    (while (and (consp x)
                (not (and (listp (car x)) (memq :with (car x)))))
      (setq x (cdr x)))
    x))

(cl-defun ymacs-company//add-backend (backend &key (main-backend-p t) (after nil))
  ;; deep copy the backends list
  (let ((backends (mapcar (lambda (x) (if (consp x) (copy-sequence x) x))
                          company-backends)))
    (if main-backend-p
        (when-let* ((parent-of-main-backend (ymacs-company//find-main-backend backends)))
          ;; remove backend first
          (setq backends (delete backend backends))
          ;; remove 'company-capf
          (setcar parent-of-main-backend
                  (delete 'company-capf (car parent-of-main-backend)))
          (if after
              (insert-after! after backend (car parent-of-main-backend))
            (cl-pushnew backend (car parent-of-main-backend))))
      (if after
          (insert-after! after backend backends)
        (cl-pushnew backend backends)))
    (setq-local company-backends backends)))

(defun ymacs-company/complete-common ()
  (interactive)
  (unless (ignore-errors (yas-expand))
    (company-complete-common)))
