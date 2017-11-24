(defvar groups-file-groups nil)

(defun groups%display-group ($group &optional echo)
  (when (cdr $group)
    (message "%s(Group %s):\n%s"
             (or echo "")
             (car $group)
             (string-join (mapcar #'abbreviate-file-name (cdr $group)) "\n"))))

(defun groups%clean-groups ()
  (setq groups-file-groups
        (loop for (name . fns) in groups-file-groups
              for new-fns = (--filter (file-exists-p it) fns)
              when new-fns
              collect (cons name new-fns))))

(defun groups%get-file-group ($file)
  (when $file
    (setq $file (file-truename $file))
    (loop for (name . files) in groups-file-groups
          when (member $file files)
          do (return (cons name files)))))

(defun groups%get-group-or-name (&optional require-match)
  (ivy-read "Group: " groups-file-groups
            :require-match require-match
            :preselect (car (groups%get-file-group (buffer-file-name)))))

(defun groups/add-file-to-group ($group $filename)
  "Add FILENAME to GROUP"
  (interactive
   (let* ((filename (buffer-file-name))
          (group-name (groups%get-group-or-name))
          (group (assoc group-name groups-file-groups)))
     (list
      (or group group-name)
      (or (and current-prefix-arg
               (not (member filename group))
               filename)
          (ivy-read "File: "
                    (--map (abbreviate-file-name (buffer-file-name it))
                           (--filter (when-let ((fn (buffer-file-name it)))
                                       (or (not group)
                                           (not (member fn group))))
                                     (buffer-list)))
                    :require-match t
                    :preselect (and (not (member filename group)) filename))))))
  (setq $filename (file-truename $filename))
  (if (stringp $group)
      (progn
        (setq $group (list $group $filename))
        (push $group groups-file-groups))
    (setcdr $group (cons $filename (cdr $group))))
  (groups%clean-groups)
  (groups%display-group $group))

(defun groups/delete-file-from-group ($group $filename)
  "Delete a FILENAME from GROUP"
  (interactive
   (let* ((group-name (groups%get-group-or-name :require-match))
          (group (assoc group-name groups-file-groups)))
     (list group
           (ivy-read "File: " (cdr-safe group)
                     :require-match t))))
  (when (and $group
             $filename
             (or current-prefix-arg
                 (yes-or-no-p (format "Delete file `%s' from GROUP %s"
                                      $filename (car $group)))))
    (setcdr $group (delete $filename (cdr $group))))
  (groups%clean-groups)
  (groups%display-group $group))

(defun groups/load-group ($group)
  "Open all files in GROUP. If `current-prefix-arg' is non-nil,
just display filenames in GROUP"
  (interactive
   (progn
     (groups%clean-groups)
     (list
      (assoc (ivy-read "Group: " groups-file-groups :require-match t)
             groups-file-groups))))
  (when $group
    (unless current-prefix-arg
      (dolist (filename (cdr $group))
        (find-file filename)))
    (groups%display-group $group
                         (unless current-prefix-arg "Opened "))))

(defun groups/kill-group ($group)
  (interactive
   (list
    (assoc (groups%get-group-or-name :require-match)
           groups-file-groups)))
  (when (and $group
             (or current-prefix-arg
                 (yes-or-no-p (format "Kill buffers of %s " (car $group)))))
    (loop for file in (cdr $group)
          for buffer = (get-file-buffer file)
          when buffer
          do (kill-buffer buffer)))
  (groups%clean-groups))

(defun groups/delete-group ($group)
  (interactive
   (list
    (assoc (groups%get-group-or-name :require-match)
           groups-file-groups)))
  (when (and $group
             (or current-prefix-arg
                 (yes-or-no-p (format "Delete GROUP %s" (car $group)))))
    (setcdr $group nil))
  (groups%clean-groups))

(add-to-list 'savehist-additional-variables 'groups-file-groups)

(define-key!
  ("C-x , a" . groups/add-file-to-group)
  ("C-x , d" . groups/delete-file-from-group)
  ("C-x , x" . groups/delete-group)
  ("C-x , k" . groups/kill-group)
  ("C-x , ," . groups/load-group))

(provide 'init-groups)
