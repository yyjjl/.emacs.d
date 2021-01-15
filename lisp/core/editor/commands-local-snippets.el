;;; -*- lexical-binding: t; -*-

;;;###autoload
(defun ymacs-editor/add-local-snippet (&optional -save-snippets)
  (interactive "P")
  (let ((template (read-string "Snippet template: "
                               (when (region-active-p)
                                 (buffer-substring-no-properties (region-beginning) (region-end)))))
        (key (read-string "Snippet key: "))
        (local-snippets-list (copy-alist ymacs-editor-local-snippets-list)))
    (-if-let (item (assoc-string key local-snippets-list))
        (when (yes-or-no-p (format "Key is used for %s, overwrite it" (cdr item)))
          (setcdr item template))
      (push (cons (substring-no-properties key) template) local-snippets-list)
      (message "Snippet %s => %s" key template))

    (setq-local ymacs-editor-local-snippets-list local-snippets-list)

    (when (or -save-snippets
              (yes-or-no-p "Save to .dir-locals.el?"))
      (save-dir-local-variables! 'ymacs-editor-local-snippets-list))))

;;;###autoload
(defun ymacs-editor/delete-local-snippet ()
  (interactive)
  (if (not ymacs-editor-local-snippets-list)
      (message "No local snippets")
    (let* ((key (completing-read! "Snippet key: " (mapcar #'car ymacs-editor-local-snippets-list)))
           (local-snippets-list (copy-alist ymacs-editor-local-snippets-list))
           (item (assoc-string key local-snippets-list)))
      (when (yes-or-no-p (format "Delete %s => %s? " (car item) (cdr item)))
        (setq-local ymacs-editor-local-snippets-list (delete item local-snippets-list))
        (save-dir-local-variables! 'ymacs-editor-local-snippets-list)))))
