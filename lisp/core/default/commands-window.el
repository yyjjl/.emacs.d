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
      (ymacs-window//split-with-size
       (completing-read! (concat "Split " (if -vertical "Vertically" "Horizontally"))
                         ymacs-window-size-list
                         (lambda ()
                           (ymacs-window//split-with-size (read-number "Float or Integer: ") -vertical))
                         "Read number")
       -vertical)
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

(defun ymacs-default//move-buffer (-direction)
  "Helper function to move the current buffer to the window in the given
   -direction (with must be 'up, 'down', 'left or 'right). An error is
   thrown, if no window exists in this direction."
  (interactive)

  (let* ((other-window (windmove-find-other-window -direction))
         (this-buffer (current-buffer)))
    (when (null other-window)
      (user-error "No %s window" -direction))
    (when (window-dedicated-p other-window)
      (user-error "The %s window is dedicated" -direction))
    (when (eq other-window (minibuffer-window))
      (user-error "The %s window is the Minibuf" -direction))
    (unless (window-parameter other-window 'ace-window-path)
      (user-error "The %s window is ignored by ace-window" -direction))

    ;; switch selected window to buffer of other window (swapping)
    (set-window-buffer (selected-window) (window-buffer other-window))
    ;; switch other window to this buffer
    (set-window-buffer other-window this-buffer)
    (select-window other-window)))

;;;###autoload
(defun ymacs-default/move-buffer ()
  "Helper function to move the current buffer to the window in the given
   -direction (with must be 'up, 'down', 'left or 'right). An error is
   thrown, if no window exists in this direction."
  (interactive)

  (require 'windmove)
  (lv-message "use h,j,k,l,<left>,<right>,<up>,<down> to move")
  (cl-letf ((move-left (interactive! (ymacs-default//move-buffer 'left)))
            (move-right (interactive! (ymacs-default//move-buffer 'right)))
            (move-down (interactive! (ymacs-default//move-buffer 'down)))
            (move-up (interactive! (ymacs-default//move-buffer 'up))))
    (let ((map (make-sparse-keymap)))
      (define-key map "h" move-left)
      (define-key map "l" move-right)
      (define-key map "j" move-down)
      (define-key map "k" move-up)
      (define-key map [left] move-left)
      (define-key map [right] move-right)
      (define-key map [down] move-down)
      (define-key map [up] move-up)
      (set-transient-map map t #'lv-delete-window))))
