;;; -*- lexical-binding: t; -*-

;;;###autoload
(defun ymacs-tools/add-local-snippet (&optional -save-snippets)
  (interactive "P")
  (let ((template (read-string "Snippet template: "
                               (when (region-active-p)
                                 (buffer-substring (region-beginning) (region-end)))))
        (key (read-string "Snippet key: "))
        (local-snippets-list (copy-alist ymacs-tools-local-snippets-list)))
    (-if-let (item (assoc-string key local-snippets-list))
        (when (yes-or-no-p (format "Key is used for %s, overwrite it" (cdr item)))
          (setcdr item template))
      (push (cons (substring-no-properties key) template) local-snippets-list)
      (message "Snippet %s => %s" key template))
    (setq-local ymacs-tools-local-snippets-list local-snippets-list)
    (when -save-snippets
      (save-dir-local-variables! 'ymacs-tools-local-snippets-list))))

;;;###autoload
(defun ymacs-tools/delete-local-snippet ()
  (interactive)
  (if (not ymacs-tools-local-snippets-list)
      (message "No local snippets")
    (let* ((key (completing-read "Snippet key: "
                                 ymacs-tools-local-snippets-list
                                 nil
                                 :require-match))
           (local-snippets-list (copy-alist ymacs-tools-local-snippets-list))
           (item (assoc-string key local-snippets-list)))
      (when (yes-or-no-p (format "Delete %s => %s? " (car item) (cdr item)))
        (setq-local ymacs-tools-local-snippets-list (delete item local-snippets-list))
        (save-dir-local-variables! 'ymacs-tools-local-snippets-list)))))
