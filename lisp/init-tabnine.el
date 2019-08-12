;;; -*- lexical-binding: t; -*-

(require-packages! company-tabnine)



(defun company*around-transform-candidates (-fn candidates)
  (if (or (functionp company-backend)
          (not (and (listp company-backend) (memq 'company-tabnine company-backend))))
      (funcall -fn candidates)
    (let ((candidates-table (make-hash-table :test #'equal))
          candidates-1
          candidates-2)
      (dolist (candidate candidates)
        (if (eq (get-text-property 0 'company-backend candidate)
                'company-tabnine)
            (unless (gethash candidate candidates-table)
              (push candidate candidates-2))
          (push candidate candidates-1)
          (puthash candidate t candidates-table)))
      (setq candidates-1 (funcall -fn (nreverse candidates-1)))
      (setq candidates-2 (nreverse candidates-2))
      (nconc (seq-take candidates-1 1)
             (seq-take candidates-2 1)
             (seq-drop candidates-1 1)
             (seq-drop candidates-2 1)))))

(defun company//check-tabnine ()
  (unless (and (require 'company-tabnine nil t)
               (company-tabnine--executable-path))
    (error "Can not load TabNine")))

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

(with-eval-after-load 'company
  (advice-add 'company--transform-candidates :around #'company*around-transform-candidates))

(provide 'init-tabnine)
