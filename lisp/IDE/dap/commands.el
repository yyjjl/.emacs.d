;; -*- lexical-binding:t -*-

;;;###autoload
(defun ymacs-dap/goto-log-buffer ()
  (interactive)
  (let ((session (dap--cur-session-or-die)))
    (when-let* ((proc (dap--debug-session-program-proc session))
                (buffer (process-buffer proc)))
      (pop-to-buffer buffer))))

;;;###autoload
(defun ymacs-dap/goto-repl-buffer ()
  (interactive)
  (dap-hydra/nil)
  (if-let ((buffer (get-buffer dap-ui--repl-buffer))
           (window (get-buffer-window buffer)))
      (if (eq window (selected-window))
          (quit-window)
        (select-window window))
    (dap-ui-repl)))
