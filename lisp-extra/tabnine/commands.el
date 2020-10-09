;;; -*- lexical-binding: t; -*-

(defun company--transform-candidates@with-tabnine (-fn candidates)
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

(defun ymacs-tabnine//check ()
  (unless (and (require 'company-tabnine nil t)
               (company-tabnine--executable-path))
    (user-error "Can not load TabNine")))

(defun ymacs-tabnine//attach-backend-to-backend (-target -attachment)
  (cond ((symbolp -target)
         `(,-target :with ,-attachment :separate))
        ((and (listp -target)
              (not (memq -attachment -target)))
         (let ((with (memq :with -target)))
           (if with
               `(,@-target ,-attachment :separate)
             `(,@-target :with ,-attachment :separate))))
        (t -target)))

(defun ymacs-tabnine//remove-backend-from-backend (-target -attachment)
  (if (symbolp -target)
      -target
    (setq -target (remove -attachment -target))
    (when (eq (car (last -target)) :separate)
      (setq -target (nbutlast -target 1)))
    (when (eq (car (last -target)) :with)
      (setq -target (nbutlast -target 1)))
    (if (null (cdr -target))
        (car -target)
      -target)))

(defsubst ymacs-tabnine//enable-p ()
  (memq
   'company-tabnine
   (car (ymacs-company//find-main-backend company-backends))))

(defun ymacs-tabnine//disable ()
  (ymacs-tabnine//check)

  (when (ymacs-tabnine//enable-p)
    (advice-remove #'company--transform-candidates
                   #'company--transform-candidates@with-tabnine)

    (let ((parent-of-main-backend (ymacs-company//find-main-backend company-backends)))
      (setcar parent-of-main-backend
              (remove 'company-tabnine (car parent-of-main-backend))))
    t))

(defun ymacs-tabnine//enable ()
  (ymacs-tabnine//check)

  (unless (ymacs-tabnine//enable-p)
    (advice-add #'company--transform-candidates
                :around #'company--transform-candidates@with-tabnine)

    (let ((parent-of-main-backend (ymacs-company//find-main-backend company-backends)))
      (insert-before! :with 'company-tabnine (car parent-of-main-backend)))
    t))

;;;###autoload
(defun ymacs-tabnine/toggle (&optional -global)
  (interactive "P")
  (if -global

      (let (hook-fn toggle-fn message-string)

        (if (memq 'ymacs-tabnine//enable prog-mode-hook)
            (setq hook-fn #'remove-hook
                  toggle-fn #'ymacs-tabnine//disable
                  message-string "TabNine is disabled globally")
          (setq hook-fn #'add-hook
                toggle-fn #'ymacs-tabnine//enable
                message-string "TabNine is enabled globally"))

        (funcall hook-fn 'prog-mode-hook 'ymacs-tabnine//enable)

        (dolist (buffer (buffer-list))
          (with-current-buffer buffer
            (when (derived-mode-p 'prog-mode)
              (funcall toggle-fn))))

        (message "%s" message-string))

    (if (ymacs-tabnine//enable-p)
        (when (ymacs-tabnine//disable)
          (message "TabNine is disabled"))
      (when (ymacs-tabnine//enable)
        (message "TabNine is enabled")))))
