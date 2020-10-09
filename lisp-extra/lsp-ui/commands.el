;; -*- lexical-binding:t -*-

;;;###autoload
(defun ymacs-dap/go-to-log-buffer ()
  (interactive)
  (let ((session (dap--cur-session-or-die)))
    (when-let* ((proc (dap--debug-session-program-proc session))
                (buffer (process-buffer proc)))
      (pop-to-buffer buffer))))

;;;###autoload
(defun ymacs-lsp-ui/toggle ()
  (interactive)
  (ymacs-lsp-ui//enable (not lsp-ui-doc-mode))
  (when (called-interactively-p 'interactive)
    (message "Modern UI %s" (if lsp-ui-doc-mode "enabled" "disabled"))))
