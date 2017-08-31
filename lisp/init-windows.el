;; When splitting window, show (other-buffer) in the new window
(defun window%wrap-split-function ($split-function)
  (lexical-let ((sf $split-function))
    (lambda ()
      (interactive)
      (funcall sf)
      (set-window-buffer (next-window) (other-buffer)))))

(global-set-key (kbd "C-x 2") (window%wrap-split-function 'split-window-vertically))
(global-set-key (kbd "C-x 3") (window%wrap-split-function 'split-window-horizontally))


;; Rearrange split windows
(defun window%split-horizontally ()
  (interactive)
  (save-excursion
    (delete-other-windows)
    (funcall (window%wrap-split-function 'split-window-horizontally))))

(defun window%split-vertically ()
  (interactive)
  (save-excursion
    (delete-other-windows)
    (funcall (window%wrap-split-function 'split-window-vertically))))

(global-set-key "\C-x|" 'window%split-horizontally)
(global-set-key "\C-x_" 'window%split-vertically)

(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(provide 'init-windows)
