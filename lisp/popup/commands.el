;;; -*- lexical-binding: t; -*-

(declare-function sdcv-search-word "sdcv")

;;;###autoload
(defun ymacs-popup/last-popup-window (&optional -arg)
  "Display last popup window"
  (interactive "P")
  (if (and (not -arg)
           (buffer-live-p (ymacs-popup//get-current-buffer)))
      (display-buffer (ymacs-popup//get-current-buffer))
    (let ((help-buffers
           (mapcar #'buffer-name
                   (cl-remove-if-not (lambda (buffer)
                                       (and (ymacs-popup//help-buffer-p buffer)
                                            (not (string-prefix-p " " (buffer-name buffer)))))
                                     (buffer-list)))))
      (display-buffer (completing-read "Popup buffer: " help-buffers nil t)))))

;;;###autoload
(defun ymacs-popup/fix-popup-window ()
  "Make a popup window not to close when `C-g' pressed"
  (interactive)
  (setq ymacs-popup--window-list
        (delq (selected-window) ymacs-popup--window-list)))

;;;###autoload
(defun ymacs-popup/popup-sdcv ()
  "Display *sdcv* buffer"
  (interactive)
  (let ((word (if (and transient-mark-mode mark-active)
                  (buffer-substring-no-properties (region-beginning) (region-end))
                (sdcv-current-word))))
    (sdcv-goto-sdcv)
    (setq word (read-string
                (format "Word (default %s): " word)
                nil nil word))
    (sdcv-search-word word)))
