;;; -*- lexical-binding: t; -*-

;;;###autoload
(defun ymacs-popup/last-popup-window (&optional -arg)
  "Display last popup window"
  (interactive "P")
  (if (and (not -arg)
           (buffer-live-p (ymacs-popup//get-current-buffer)))
      (display-buffer (ymacs-popup//get-current-buffer))
    (let ((help-buffers
           (mapcar #'buffer-name
                   (cl-remove-if-not
                    (lambda (buffer)
                      (with-current-buffer buffer
                        (plist-get ymacs-popup--matched-rule :autoclose)))
                    (buffer-list)))))
      (display-buffer (completing-read "Popup buffer: " help-buffers nil t)))))

;;;###autoload
(defun ymacs-popup/delete-other-window ()
  (interactive)
  (let ((window (selected-window)))
    (when (or (not (window-parameter window 'window-side))
              (yes-or-no-p "Are you sure to make side window the only window?"))
      (set-window-parameter window 'window-side nil)
      (call-interactively #'delete-other-windows))))

;;;###autoload
(defun ymacs-popup/display-popup-window ()
  (interactive)
  (let ((help-buffers
         (mapcar #'buffer-name
                 (cl-remove-if-not
                  (lambda (buffer)
                    (with-current-buffer buffer
                      (or (plist-get ymacs-popup--matched-rule :autoclose)
                          (plist-get ymacs-popup--matched-rule :dedicated))))
                  (buffer-list)))))
    (display-buffer (completing-read "Popup buffer: " help-buffers nil t))))
