;;; -*- lexical-binding: t; -*-

;;;###autoload
(defun ymacs-popups/last-popup-window ()
  "Display last popup window"
  (interactive)
  (if (buffer-live-p shackle-last-buffer)
      (display-buffer shackle-last-buffer)
    (message "Last buffer killed !!!")
    (ymacs-popups/display-buffer)))

;;;###autoload
(defun ymacs-popups/display-buffer ()
  (interactive)
  (let ((ivy-use-virtual-buffers nil))
    (display-buffer
     (completing-read "Buffer: "
                      #'internal-complete-buffer
                      nil
                      :require-match))))

;;;###autoload
(defun ymacs-popups/fix-popup-window ()
  "Make a popup window not to close when `C-g' pressed"
  (interactive)
  (let ((window (selected-window)))
    (setq ymacs-popups--window-list
          (--filter (not (equal (car it) window)) ymacs-popups--window-list))))

;;;###autoload
(defun ymacs-popups/popup-sdcv ()
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
