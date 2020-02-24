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

(with-eval-after-load 'company
  (advice-add 'company--transform-candidates
              :around #'company*around-transform-candidates))

(provide 'init-tabnine)
