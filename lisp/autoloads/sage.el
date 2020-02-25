;;; -*- lexical-binding: t; -*-
;;;###autoload
(defun sage/clean-current-buffer ()
  (interactive)
  (sage-shell:-delete-output (point-min))
  (sage-shell-edit:delete-temp-dir))

;;;###autoload
(defun sage/pop-to-source-buffer ()
  (interactive)
  (unless sage-shell:source-buffer
    (setq sage-shell:source-buffer
          (completing-read "Buffer:"
                           (mapcar
                            #'buffer-name
                            (--filter (eq 'sage-shell:sage-mode
                                          (buffer-local-value 'major-mode it))
                                      (buffer-list)))
                           nil :require-match)))
  (when sage-shell:source-buffer
    (pop-to-buffer sage-shell:source-buffer)))
