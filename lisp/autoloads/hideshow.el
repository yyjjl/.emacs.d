;;; -*- lexical-binding: t; -*-

(defvar hs-persistent-file "fold.el")
(defvar hs-persistent-table (make-hash-table :test #'equal))

(defun hs//get-overlays ()
  (cl-loop for overlay in (overlays-in (point-min) (point-max))
        when (overlay-get overlay 'hs)
        collect overlay))

(defun hs//load-persistent-table ()
  (core//load-variable 'hs-persistent-table hs-persistent-file)
  (let ((table hs-persistent-table))
    (when hs-persistent-table
      (dolist (key (hash-table-keys hs-persistent-table))
        (when (file-exists-p key)
          (puthash key (gethash key hs-persistent-table) table))))
    (setq hs-persistent-table table)))

(defun hs//save-folds (&optional buffer-or-name)
  "Save folds in BUFFER-OR-NAME, which should have associated file.

BUFFER-OR-NAME defaults to current buffer."
  (when hs-minor-mode
    (with-current-buffer (or buffer-or-name (current-buffer))
      (let ((filename (buffer-file-name))
            information)
        (when filename
          (setq filename (substring-no-properties filename))
          (dolist (ov (hs//get-overlays))
            (push (cons (overlay-start ov) (overlay-end ov)) information))
          (if information
              (progn
                (setq information
                      (sort information (lambda (x y) (< (cdr x) (cdr y)))))
                (push (file-modification-time! filename) information)
                (puthash filename information hs-persistent-table))
            (when (gethash filename hs-persistent-table)
              (remhash filename hs-persistent-table))))))))

(defun hs//restore-folds (&optional buffer-or-name)
  "Restore folds in BUFFER-OR-NAME, if they have been saved.

BUFFER-OR-NAME defaults to current buffer."
  (when hs-minor-mode
    (with-current-buffer (or buffer-or-name (current-buffer))
      (let ((filename (buffer-file-name)))
        (when filename
          (setq filename (substring-no-properties filename))
          (when-let ((information (gethash filename hs-persistent-table))
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

(defun hs|kill-emacs-hook ()
  "Traverse all buffers and try to save their folds."
  (mapc #'hs//save-folds (buffer-list))
  (core//save-variable 'hs-persistent-table hs-persistent-file))

;;;###autoload
(define-minor-mode hs-persistent-mode
  "Toggle `hs-persistent-mode' minor mode."
  :global nil
  (let ((fnc (if hs-persistent-mode #'add-hook #'remove-hook)))
    (funcall fnc 'hs-minor-mode-hook #'hs//restore-folds)
    (funcall fnc 'kill-buffer-hook #'hs//save-folds)
    (funcall fnc 'kill-emacs-hook #'hs|kill-emacs-hook))
  (when hs-persistent-mode
    (mapc #'hs//restore-folds (buffer-list))))

(hs//load-persistent-table)