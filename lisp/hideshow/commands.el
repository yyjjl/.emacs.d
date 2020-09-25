;;; -*- lexical-binding: t; -*-

(defun ymacs-hideshow//get-overlays ()
  (cl-loop for overlay in (overlays-in (point-min) (point-max))
           when (overlay-get overlay 'hs)
           collect overlay))

(defun ymacs-hideshow//record-folds (&optional -buffer-or-name)
  "Save folds in BUFFER-OR-NAME, which should have associated file.

BUFFER-OR-NAME defaults to current buffer."
  (when hs-minor-mode
    (with-current-buffer (or -buffer-or-name (current-buffer))
      (let ((filename (buffer-file-name))
            information)
        (when filename
          (setq filename (substring-no-properties filename))
          (dolist (ov (ymacs-hideshow//get-overlays))
            (push (cons (overlay-start ov) (overlay-end ov)) information))
          (if information
              (progn
                (setq information
                      (sort information (lambda (x y) (< (cdr x) (cdr y)))))
                (push (file-modification-time! filename) information)
                (puthash filename information ymacs-hideshow-persistent-table))
            (when (gethash filename ymacs-hideshow-persistent-table)
              (remhash filename ymacs-hideshow-persistent-table))))))))

(defun ymacs-hideshow//restore-folds (&optional -buffer-or-name)
  "Restore folds in BUFFER-OR-NAME, if they have been saved.

BUFFER-OR-NAME defaults to current buffer."
  (when hs-minor-mode
    (with-current-buffer (or -buffer-or-name (current-buffer))
      (let ((filename (buffer-file-name)))
        (when filename
          (setq filename (substring-no-properties filename))
          (when-let ((information (gethash filename ymacs-hideshow-persistent-table))
                     (mtime (car information))
                     (regions (and mtime
                                   (equal (file-modification-time! filename)
                                          mtime)
                                   (cdr information))))
            (save-excursion
              (cl-loop for (beg . end) in regions
                       do (progn
                            (goto-char beg)
                            (hs-hide-block))))))))))

(defun ymacs-hideshow//load-folds-from-file ()
  (ymacs//load-variable 'ymacs-hideshow-persistent-table
                        ymacs-hideshow-persistent-file)

  (let ((table (make-hash-table :test #'equal)))
    (when ymacs-hideshow-persistent-table
      (dolist (key (hash-table-keys ymacs-hideshow-persistent-table))
        (when (file-exists-p key)
          (puthash key (gethash key ymacs-hideshow-persistent-table) table))))
    (setq ymacs-hideshow-persistent-table table)

    (mapc #'ymacs-hideshow//restore-folds (buffer-list))))

(defun ymacs-hideshow//save-folds-to-file ()
  "Traverse all buffers and try to save their folds."
  (mapc #'ymacs-hideshow//record-folds (buffer-list))
  (ymacs//save-variable 'ymacs-hideshow-persistent-table
                        ymacs-hideshow-persistent-file))

;;;###autoload
(define-minor-mode ymacs-hideshow/persistent-mode
  "Toggle `ymacs-hideshow/persistent-mode' minor mode."
  :global nil
  (if ymacs-hideshow/persistent-mode
      (progn
        (ymacs-hideshow//load-folds-from-file)

        (add-hook 'hs-minor-mode-hook #'ymacs-hideshow//restore-folds)
        (add-hook 'kill-buffer-hook #'ymacs-hideshow//record-folds)
        (add-hook 'kill-emacs-hook #'ymacs-hideshow//save-folds-to-file))
    (progn
      (remove-hook 'hs-minor-mode-hook #'ymacs-hideshow//restore-folds)
      (remove-hook 'kill-buffer-hook #'ymacs-hideshow//record-folds)
      (remove-hook 'kill-emacs-hook #'ymacs-hideshow//save-folds-to-file)

      (ymacs-hideshow//save-folds-to-file))))
