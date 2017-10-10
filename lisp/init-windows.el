;; When splitting window, show (other-buffer) in the new window

(defvar window-size-list '("0.25" "0.382" "0.5" "0.618" "0.7"))
(defun window%split-with-size ($size &optional $vertical?)
  "Set new window size to $SIZE"
  (let ((window-size (if $vertical?
                         (window-body-height)
                       (window-body-width))))
    (funcall (if $vertical?
                 #'split-window-vertically
               #'split-window-horizontally)
             (cond ((integerp $size) (- window-size (abs $size)))
                   ((floatp $size) (round (* (- 1 $size) window-size)))
                   (t nil)))
    (set-window-buffer (next-window) (other-buffer))
    (when (or (and (floatp $size) (> $size 0.5))
              (and (integerp $size) (> $size (* 0.5 window-size))))
      (if $vertical?
          (windmove-down)
        (windmove-right)))))

(defun window/split-vertically (&optional $arg)
  (interactive "P")
  (window%split-with-size (when $arg (string-to-number
                                      (completing-read "Float or Integer: "
                                                       window-size-list)))
                          :vertial))

(defun window/split-horizontally (&optional $arg)
  (interactive "P")
  (window%split-with-size (when $arg (string-to-number
                                      (completing-read "Float or Integer: "
                                                       window-size-list)))))

;; Rearrange split windows
(defun window/force-split-horizontally (&optional $arg)
  (interactive "P")
  (save-excursion
    (delete-other-windows)
    (window/split-horizontally $arg)))

(defun window/force-split-vertically (&optional $arg)
  (interactive "P")
  (save-excursion
    (delete-other-windows)
    (window/split-vertically $arg)))

(defun window%split-n ($window $n $align)
  (when (> $n 1)
    (let ((split-function (if (eq $align :vertical)
                              #'split-window-vertically
                            #'split-window-horizontally))
          (size (if (eq $align :vertical)
                    (window-height $window)
                  (window-width $window))))
      (dotimes (_ (- $n 1))
        (with-selected-window $window
          (setq $window
                (funcall split-function (round (/ size $n)))))))))

(defun window/split-window-two-panel ($align $num1 $num2 ratio)
  (interactive (list (read-char "`-' or `|' :")
                     (max (read-number "First: ") 1)
                     (max (read-number "Second: ") 1)
                     (if current-prefix-arg
                         (min 0.7 (max 0.3 (read-number "Ratio(0.3-0.7): ")))
                       0.5)))
  (delete-other-windows)
  (let ((orig-window (selected-window))
        new-window
        align)
    (cond
     ((eq $align ?-)
      (setq new-window (split-window-vertically
                        (round (* ratio (window-height))))
            align :horizontal))
     ((eq $align ?|)
      (setq new-window (split-window-horizontally
                        (round (* ratio (window-width))))
            align :vertical))
     (t
      (message "Nothing to do !!!")))
    (when align
      (window%split-n orig-window $num1 align)
      (window%split-n new-window $num2 align))))

(define-key! :prefix "C-x"
  ("2" . window/split-vertically)
  ("3" . window/split-horizontally)
  ("|" . window/force-split-horizontally)
  ("_" . window/force-split-vertically))

(provide 'init-windows)
