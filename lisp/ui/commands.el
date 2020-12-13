;;; -*- lexical-binding: t; -*-

;;;###autoload
(defun ymacs-ui/toggle-winum-scope ()
  (interactive)
  (setq winum-scope (if (eq winum-scope 'frame-local)
                        'visible
                      'frame-local))
  (message "Current winum scope: %s" (upcase (symbol-name winum-scope)))
  (dolist (frame (frame-list))
    (select-frame frame)
    (winum--update))
  (force-mode-line-update))

;;;###autoload
(define-minor-mode ymacs-ui/view-code-mode
  "View code"
  :init-value nil
  (let ((switch (if ymacs-ui/view-code-mode 1 -1)))
    (highlight-indent-guides-auto-set-faces)
    (cl-loop for (condition . modes) in ymacs-ui-view-code-modes
             when (or (eq condition t)
                      (and (symbolp condition) (symbol-value condition)))
             do (dolist (mode modes)
                  (funcall mode switch)))))

