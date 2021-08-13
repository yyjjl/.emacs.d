;;; -*- lexical-binding: t; -*-

;;;###autoload
(defun ymacs-popup/last-popup-window (&optional -arg)
  "Display last popup window"
  (interactive "P")
  (if (and (not -arg)
           (buffer-live-p (ymacs-popup//get-current-buffer)))
      (display-buffer (ymacs-popup//get-current-buffer))
    (call-interactively #'pop-to-buffer)))

;;;###autoload
(defun ymacs-popup/delete-other-window ()
  (interactive)
  (let ((window (selected-window)))
    (when (or (not (window-parameter window 'window-side))
              (yes-or-no-p "Are you sure to make side window the only window?"))
      (set-window-parameter window 'window-side nil)
      (call-interactively #'delete-other-windows))))
