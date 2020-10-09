;;; -*- lexical-binding: t; -*-

(defun ymacs-ivy/display-function-window (text)
  (let ((buffer (get-buffer-create ymacs-ivy--candidate-buffer))
        (search-commands))

    (with-current-buffer (window-buffer (minibuffer-window))
      (setq line-spacing nil))

    (with-current-buffer buffer
      (setq cursor-type nil)
      (setq display-fill-column-indicator nil)
      (setq display-line-numbers nil)
      (setq header-line-format nil)
      (setq mode-line-format nil)
      (setq tab-line-format nil)
      (setq truncate-lines t)

      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (string-trim-left text "\n"))
        (goto-char (point-min))))

    (with-ivy-window
      (fit-window-to-buffer
       (display-buffer
        buffer
        '((display-buffer-reuse-window
           display-buffer-pop-up-window)
          (inhibit-same-window . t)
          (side . bottom)))
       nil
       ymacs-ivy--candidate-window-height))

    (let ((window (get-buffer-window buffer)))
      ;; make window grows only
      (set-window-dedicated-p window t)
      (set-window-parameter window 'no-delete-other-window t)
      (set-window-parameter window 'no-other-window t)

      (setq ymacs-ivy--candidate-window-height (window-height window)))))
