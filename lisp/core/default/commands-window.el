;;; -*- lexical-binding: t; -*-

(defvar ymacs-window-size-list '(0.25 0.382 0.5 0.618 0.7))
(defun ymacs-window//split-with-size (-size &optional -vertical)
  "Set new window size to -SIZE"
  (let ((window-size (if -vertical
                         (window-body-height)
                       (window-body-width))))
    (funcall (if -vertical
                 #'split-window-vertically
               #'split-window-horizontally)
             (cond ((integerp -size) (- window-size (abs -size)))
                   ((floatp -size) (round (* (- 1 -size) window-size)))
                   (t nil)))
    (set-window-buffer (next-window) (other-buffer))
    (when (or (and (floatp -size) (> -size 0.5))
              (and (integerp -size) (> -size (* 0.5 window-size))))
      (if -vertical
          (windmove-down)
        (windmove-right)))))

;;;###autoload
(defun ymacs-window/split-vertically (&optional -arg)
  (interactive "P")
  (ymacs-window/split-horizontally -arg t))

;;;###autoload
(defun ymacs-window/split-horizontally (&optional -arg -vertical)
  (interactive "P")
  (when (eq (selected-window)
            (ymacs-popup//get-term-window))
    (user-error "Terminal window can not split"))
  (if -arg
      (completing-read!
       :-prompt "Split Vertically"
       :-collection ymacs-window-size-list
       :-action (lambda (-size)
                  (ymacs-window//split-with-size -size -vertical))
       :-return-prompt "read number"
       :-return-action
       (lambda ()
         (ymacs-window//split-with-size (read-number "Float or Integer: ") -vertical)))
    (ymacs-window//split-with-size nil -vertical)))

;; Rearrange split windows
;;;###autoload
(defun ymacs-window/force-split-horizontally (&optional -arg)
  (interactive "P")
  (save-excursion
    (delete-other-windows)
    (ymacs-window/split-horizontally -arg)))

;;;###autoload
(defun ymacs-window/force-split-vertically (&optional -arg)
  (interactive "P")
  (save-excursion
    (delete-other-windows)
    (ymacs-window/split-vertically -arg)))
