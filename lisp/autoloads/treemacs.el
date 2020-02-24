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
(defun treemacs*around-select-window-by-number (fn &optional arg)
  "Jump to window 1 or treemacs-window."
  (interactive)
  (if (and (eq major-mode 'treemacs-mode)
           (integerp arg)
           (> arg 1)
           (not (winum-get-window-by-number arg)))
      (funcall fn (1- arg))
    (funcall fn arg)))
