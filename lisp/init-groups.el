(defvar buffer-group|groups nil)
(defvar buffer-group|current-group nil)
(defvar buffer-group|last-group nil)

(defun buffer-group|make-cache ()
  (let ((cache (make-hash-table :test #'equal)))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (cond ((eq major-mode 'dired-mode)
               (puthash default-directory buf cache))
              ((buffer-file-name)
               (puthash (buffer-file-name) buf cache)))))
    cache))

(defun buffer-group|get-group (&optional create-p)
  (let* ((group (completing-read "Group: "
                                 (mapcar #'car buffer-group|groups)
                                 nil (not create-p)
                                 (and buffer-group|current-group
                                      buffer-group|last-group)))
         (value (assoc group buffer-group|groups)))
    (when (not value)
      (setq value (list group))
      (push value buffer-group|groups))
    value))

(defun buffer-group|kill-buffers (files)
  (let ((cache (buffer-group|make-cache)))
    (dolist (file files)
      (let ((buf (gethash file cache)))
        (when buf
          (message "Kill buffer %s" buf)
          (kill-buffer buf))))))

(defun buffer-group|create-or-switch ()
  (interactive)
  (let ((g (buffer-group|get-group t))
        files)
    (when g
      (progn
        (dolist (file (cdr g))
          (condition-case-unless-debug err
              (progn (find-file file)
                     (push file files))
            (error (message "Error: %s" err)
                   (message "File %s removed from group" file))))
        (setcdr g files)
        (setq buffer-group|last-group buffer-group|current-group)
        (setq buffer-group|current-group (car g))
        (message "Switch to group %s" (car g))))))

(defun buffer-group|delete-group (kill-buffer-p)
  (interactive "P")
  (let ((g (buffer-group|get-group nil)))
    (when (and g (yes-or-no-p (format "Delete group %s?" (car g))))
      (progn
        (when (or kill-buffer-p
                  (yes-or-no-p "Kill buffers in group too?"))
          (buffer-group|kill-buffers (cdr g)))

        (setq buffer-group|groups (delete g buffer-group|groups))

        (when (equal (car g) buffer-group|current-group)
          (setq buffer-group|last-group buffer-group|current-group)
          (setq buffer-group|current-group nil))
        (when (equal (car g) buffer-group|last-group)
          (setq buffer-group|last-group nil))
        (message "Delete finished")))))

(defun buffer-group|add-buffer (buffer)
  (interactive (list (current-buffer)))
  (let ((g (assoc buffer-group|current-group
                  buffer-group|groups)))
    (if g
        (with-current-buffer buffer
          (let ((file (buffer-file-name)))
            (if (not file)
                (message "Non-file buffer can not be added !!!")
              (unless (member file (cdr g))
                (setcdr g (cons file (cdr g)))
                (message "Buffer added to group %s"
                         buffer-group|current-group)))))
      (message "No group selected !!!"))))

(defun buffer-group|remove-bufer (buffer)
  (interactive (list (current-buffer)))
  (let ((g (assoc buffer-group|current-group
                  buffer-group|groups)))
    (if g
        (with-current-buffer buffer
          (let ((file (buffer-file-name)))
            (if (member file (cdr g))
                (progn
                  (setcdr g (remove file g))
                  (message "Buffer removed from group %s"
                           buffer-group|current-group)
                  (when (yes-or-no-p "Kill buffer too?")
                    (kill-buffer)))
              (message "Buffer is not in current group %s"
                       buffer-group|current-group))))
      (message "No group selected !!!"))))

(defun buffer-group|kill-group ()
  (interactive)
  (let ((g (buffer-group|get-group nil)))
    (if (and (cdr g) (yes-or-no-p "Kill buffers in group?"))
        (buffer-group|kill-buffers (cdr g))
      (message "Nothing to do"))))


(defun buffer-group|switch-buffer ()
  (interactive)
  (let ((g (assoc buffer-group|current-group
                  buffer-group|groups)))
    (if g
        (let ((cache (buffer-group|make-cache)))
          (switch-to-buffer
           (completing-read
            "Buffer: "
            (delete nil (mapcar (lambda (name)
                                  (buffer-name (gethash name cache)))
                                (cdr g)))
            nil t)))
      (message "No group selected !!!"))))

(define-keys :prefix "C-c w"
  ("c" . buffer-group|create-or-switch)
  ("X" . buffer-group|delete-group)
  ("k" . buffer-group|kill-group)
  ("a" . buffer-group|add-buffer)
  ("x" . buffer-group|remove-buffer)
  ("b" . buffer-group|switch-buffer))

(provide 'init-groups)
