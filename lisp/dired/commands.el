;;; -*- lexical-binding: t; -*-

;;;###autoload
(defun ymacs-dired/open-externally ()
  "Open the current file or dired marked files in external app.
The app is chosen from your OS's preference."
  (interactive)
  (let* ((file-list (if (eq major-mode 'dired-mode)
                        (dired-get-marked-files)
                      (list (buffer-file-name))))
         (do-it-p (or (<= (length file-list) 5)
                      (y-or-n-p "Open more than 5 files? "))))
    (when do-it-p
      (open! file-list))))
