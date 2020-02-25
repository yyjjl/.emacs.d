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
