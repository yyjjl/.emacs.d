;; -*- lexical-binding:t -*-

;;;###autoload
(defun ymacs-dap/go-to-log-buffer ()
  (interactive)
  (let ((session (dap--cur-session-or-die)))
    (when-let* ((proc (dap--debug-session-program-proc session))
                (buffer (process-buffer proc)))
      (pop-to-buffer buffer))))
