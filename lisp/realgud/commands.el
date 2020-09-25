;;; -*- lexical-binding: t; -*-

;;;###autoload
(defun ymacs-realgud/jump-to-cmdbuf ()
  (interactive)
  (let ((cmdbuf (realgud-get-cmdbuf))
        (display-buffer-overriding-action
         '(display-buffer-use-some-window . ((inhibit-same-window . t)))))
    (unless (eq cmdbuf (current-buffer))
      (if (buffer-live-p cmdbuf)
          (if-let (window (get-buffer-window cmdbuf))
              (select-window window)
            (pop-to-buffer cmdbuf))
        (message "Command buffer is dead.")))))

;;;###autoload
(defun ymacs-realgud/jump-to-srcbuf ()
  (interactive)
  (let ((srcbuf (realgud-get-current-srcbuf))
        (display-buffer-overriding-action
         '(display-buffer-use-some-window . ((inhibit-same-window . t)))))
    (unless (eq srcbuf (current-buffer))
      (if (buffer-live-p srcbuf)
          (if-let (window (get-buffer-window srcbuf))
              (select-window window)
            (pop-to-buffer srcbuf))
        (message "Source buffer is dead.")))))

;;;###autoload
(defun ymacs-realgud/cmd-breakpoints (-arg)
  (interactive "P")
  (realgud:cmd-run-command -arg "breakpoints"))

;;;###autoload
(defun ymacs-realgud/cmd-display (-arg)
  (interactive "P")
  (let ((thing (when (not -arg)
                 (if (use-region-p)
                     (buffer-substring-no-properties (region-beginning)
                                                     (region-end))
                   (read-string "Thing: ")))))
    (realgud:cmd-run-command thing (if thing "display" "display-all"))))

;;;###autoload
(defun ymacs-realgud/restore-breakpoints ()
  (interactive)
  (when-let (cmdbuf (realgud-get-cmdbuf))
    (with-current-buffer-safe cmdbuf
      (unless ymacs-realgud--saved-breakpoints
        (user-error "No saved breakpoints."))
      (dolist (bp ymacs-realgud--saved-breakpoints)
        (let ((cmd (format "break %s:%s" (car bp) (cdr bp))))
          (realgud-command cmd nil t)
          (sit-for 0.5))))))
