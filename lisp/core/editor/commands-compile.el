;;; -*- lexical-binding: t; -*-

;;;###autoload
(defun ymacs-editor/select-error-buffer ()
  (interactive)
  (let* ((error-buffer (next-error-find-buffer))
         (buffers (ymacs-editor//get-other-error-buffers (current-buffer) error-buffer))
         (buffer
          (cdr-safe
           (if (> (length buffers) 1)
               (completing-read! (format "(current: %s): "
                                         (when error-buffer (buffer-name error-buffer)))
                                 buffers)
             (car buffers)))))
    (unless buffer
      (user-error "Nothing to do"))
    (message "Set `%s' as error buffer" buffer)
    (setq next-error-buffer buffer)
    (setq next-error-last-buffer buffer)))
