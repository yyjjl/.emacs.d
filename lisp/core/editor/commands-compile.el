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

;;;###autoload
(defun ymacs-editor/grep-next-error-no-select (&optional -n)
  (interactive "p")
  (let ((colum (current-column)))
    (next-error-no-select -n)
    (move-to-column colum)))

;;;###autoload
(defun ymacs-editor/grep-previous-error-no-select (&optional -n)
  (interactive "p")
  (ymacs-editor/grep-next-error-no-select (- (or -n 1))))

;;;###autoload
(defun ymacs-editor/grep-display-error ()
  (interactive)
  (setq compilation-current-error (point))
  (ymacs-editor/grep-next-error-no-select 0))
