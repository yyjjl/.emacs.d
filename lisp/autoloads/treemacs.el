;;; -*- lexical-binding: t; -*-

;;;###autoload
(defun treemacs/select-window ()
  (interactive)
  (if (eq major-mode 'treemacs-mode)
      (other-window 1)
    (treemacs-select-window)))

;;;###autoload
(defun treemacs/select-window-1 ()
  "Jump to window 1 or treemacs-window."
  (interactive)
  (let ((windows (window-list))
        (treemacs-window (treemacs-get-local-window)))
    (if (or (and (= 2 (length windows))
                 (memq treemacs-window windows)
                 (not (eq major-mode 'treemacs-mode)))
            (and treemacs-window
                 (equal 1 (winum-get-number))))
        (treemacs-select-window)
      (winum-select-window-by-number 1))))

;;;###autoload
(defun treemacs/switch (-arg)
  "-arg = 16  ---> call treemacs
-arg = 4 ---> call ivy read to select a treemacs window
otherwise ---> jump to next treemacs window"
  (interactive "P")
  (let ((windows (--filter (with-selected-window it (derived-mode-p 'treemacs-mode))
                           (window-list))))
    (if (or (equal -arg '(16)) (null windows))
        (treemacs)
      (let ((next-window (or (cadr (memq (selected-window) windows))
                             (car windows)))
            (collections (--map (cons (buffer-name (window-buffer it)) it)
                                windows)))
        (if (equal -arg '(4))
            (ivy-read "Jump to window: " collections
                      :preselect (buffer-name (window-buffer next-window))
                      :require-match t
                      :action (lambda (x) (select-window (cdr x))))
          (select-window next-window))))))
