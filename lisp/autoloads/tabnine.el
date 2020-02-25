;;; -*- lexical-binding: t; -*-

(defun company//check-tabnine ()
  (unless (and (require 'company-tabnine nil t)
               (company-tabnine--executable-path))
    (user-error "Can not load TabNine")))

(defun company//attach-backend-to-backend (target attachment)
  (cond ((symbolp target)
         `(,target :with ,attachment :separate))
        ((and (listp target)
              (not (memq attachment target)))
         (let ((with (memq :with target)))
           (if with
               `(,@target ,attachment :separate)
             `(,@target :with ,attachment :separate))))
        (t target)))

(defun company//remove-backend-from-backend (target attachment)
  (if (symbolp target)
      target
    (setq target (remove attachment target))
    (when (eq (car (last target)) :separate)
      (setq target (nbutlast target 1)))
    (when (eq (car (last target)) :with)
      (setq target (nbutlast target 1)))
    (if (null (cdr target))
        (car target)
      target)))

(defsubst company//tabnine-enable-p ()
  (let ((parent-of-main-backend (company//find-main-backend company-backends)))
    (memq 'company-tabnine (car parent-of-main-backend))))

(defun company//disable-tabnine ()
  (company//check-tabnine)

  (when (company//tabnine-enable-p)
    (let ((parent-of-main-backend (company//find-main-backend company-backends)))
      (setcar parent-of-main-backend
              (remove 'company-tabnine (car parent-of-main-backend))))
    t))

(defun company//enable-tabnine ()
  (company//check-tabnine)

  (unless (company//tabnine-enable-p)
    (let ((parent-of-main-backend (company//find-main-backend company-backends)))
      (insert-before! :with 'company-tabnine (car parent-of-main-backend)))
    t))

;;;###autoload
(defun company/toggle-tabnine (&optional -global)
  (interactive "P")
  (if -global
      (let (hook-fn toggle-fn message-string)
        (if (memq 'company//enable-tabnine prog-mode-hook)
            (setq hook-fn #'remove-hook
                  toggle-fn #'company//disable-tabnine
                  message-string "TabNine is disabled globally")
          (setq hook-fn #'add-hook
                toggle-fn #'company//enable-tabnine
                message-string "TabNine is enabled globally"))
        (funcall hook-fn 'prog-mode-hook 'company//enable-tabnine)
        (dolist (buffer (buffer-list))
          (with-current-buffer buffer
            (when (derived-mode-p 'prog-mode)
              (funcall toggle-fn))))
        (message "%s" message-string))
    (if (company//tabnine-enable-p)
        (when (company//disable-tabnine)
          (message "TabNine is disabled"))
      (when (company//enable-tabnine)
        (message "TabNine is enabled")))))
