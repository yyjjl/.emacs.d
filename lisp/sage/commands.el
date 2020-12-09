;;; -*- lexical-binding: t; -*-

;;;###autoload
(defun ymacs-sage/clean-current-buffer ()
  (interactive)
  (sage-shell:-delete-output (point-min))
  (sage-shell-edit:delete-temp-dir))

;;;###autoload
(defun ymacs-sage/pop-to-source-buffer ()
  (interactive)
  (unless ymacs-sage-shell:source-buffer
    (setq ymacs-sage-shell:source-buffer
          (completing-read
           "Buffer:"
           (mapcar
            #'buffer-name
            (--filter (eq 'sage-shell:sage-mode
                          (buffer-local-value 'major-mode it))
                      (buffer-list)))
           nil
           :require-match)))
  (when ymacs-sage-shell:source-buffer
    (pop-to-buffer ymacs-sage-shell:source-buffer)))
