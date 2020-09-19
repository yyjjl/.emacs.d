;; -*- lexical-binding:t -*-

;;;###autoload
(defun lsp/remove-session-folder (-remove-invalid)
  (interactive "P")
  (require 'dired)
  (let* ((session (lsp-session))
         invalid-folders
         valid-folders)
    (cl-loop for folder in (lsp-session-folders session)
             if (file-exists-p folder)
             do (push folder valid-folders)
             else do (push folder invalid-folders))
    (if -remove-invalid
        (if invalid-folders
            (when (dired-mark-pop-up
                   " *lsp-remove*" 'delete invalid-folders 'yes-or-no-p
                   "Remove these folders ")
              (setf (lsp-session-folders (lsp-session)) valid-folders)
              (lsp--persist-session (lsp-session)))
          (message "Nothing to remove."))
      (let ((folder (completing-read "Folder to remove" valid-folders nil t)))
        (setf (lsp-session-folders (lsp-session)) (delete folder valid-folders))
        (lsp--persist-session (lsp-session))))))

;;;###autoload
(defun dap/go-to-log-buffer ()
  (interactive)
  (let ((session (dap--cur-session-or-die)))
    (when-let* ((proc (dap--debug-session-program-proc session))
                (buffer (process-buffer proc)))
      (pop-to-buffer buffer))))
