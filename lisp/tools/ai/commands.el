;;; -*- lexical-binding: t; -*-

;;;###autoload
(defun ymacs-ai/agent-create-or-toggle (&optional -new)
  (interactive "P")
  (if -new
      (claude-code-ide--start-session)
    (condition-case _err
        (claude-code-ide-switch-to-buffer)
      (user-error
       (claude-code-ide--start-session)))))
