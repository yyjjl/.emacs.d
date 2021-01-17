;;; -*- lexical-binding: t; -*-

;;;###autoload
(defun ymacs-ui/aw-select-window ()
  "Select the specified window."
  (interactive)
  (let ((path (-> (this-command-keys-vector)
                  key-description
                  (split-string "-")
                  (elt 1))))
    (unless (cl-loop
             for window in (aw-window-list)
             when (and (window-live-p window)
                       (equal path (window-parameter window 'ace-window-path)))
             return (aw-switch-to-window window))
      (message "No specified window: %d" path))))

;;;###autoload
(defun ymacs-ui/toggle-aw-scope ()
  (interactive)
  (setq aw-scope
        (completing-read! "Scope: " (remove aw-scope '(visible global frame))))
  (aw-update)
  (force-mode-line-update t)
  (message "Current AW scope: %s" (upcase (symbol-name aw-scope))))

;;;###autoload
(define-minor-mode ymacs-ui/view-code-mode
  "View code"
  :group 'ymacs
  :init-value nil
  (let ((switch (if ymacs-ui/view-code-mode 1 -1)))
    (cl-loop for (condition . modes) in ymacs-ui-view-code-modes
             when (or (eq condition t)
                      (and (symbolp condition) (symbol-value condition)))
             do (dolist (mode modes)
                  (funcall mode switch)))))

