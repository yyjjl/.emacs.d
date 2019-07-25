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

(defun company//tabnine-enable-p ()
  (let ((first-backend (car company-backends)))
    (and (listp first-backend)
         (memq 'company-tabnine first-backend))))

(defun company//disable-tabnine ()
  (company//check-tabnine)

  (when (company//tabnine-enable-p)
    (setq-local company-backends
                (mapcar (lambda (backend)
                          (company//remove-backend-from-backend backend 'company-tabnine))
                        (remove 'company-tabnine company-backends)))
    (message "TabNine disabled")))

(defun company//enable-tabnine ()
  (company//check-tabnine)

  (unless (company//tabnine-enable-p)
    (let* ((backends (copy-sequence company-backends))
           (capf (memq 'company-capf backends)))
      (when capf
        (setcar capf '(company-capf company-tabnine :separate)))
      (setq-local company-backends
                  (cons (company//attach-backend-to-backend (car company-backends) 'company-tabnine)
                        (cdr backends))))
    (message "TabNine enable")))

(defun company/toggle-tabnine ()
  (interactive)
  (if (company//tabnine-enable-p)
      (company//disable-tabnine)
    (company//enable-tabnine)))

(define-hook! company|tabnine-setup (prog-mode-hook)
  (when (not (buffer-temporary-p))
    (let ((buffer (current-buffer)))
      (run-with-idle-timer
       1 nil
       (lambda ()
         (with-current-buffer buffer
           (ignore-errors (company//enable-tabnine))))))))

(with-eval-after-load 'company
  (advice-add 'company--transform-candidates :around #'company*around-transform-candidates))

(provide 'init-tabnine)
