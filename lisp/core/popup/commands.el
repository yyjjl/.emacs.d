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
(defun ymacs-popup/fix-popup-window ()
  "Make a popup window not to close when `C-g' pressed"
  (interactive)
  (setq ymacs-popup--window-list
        (delq (selected-window) ymacs-popup--window-list)))