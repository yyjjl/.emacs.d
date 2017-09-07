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
(defun window/split-horizontally ()
  (interactive)
  (save-excursion
    (delete-other-windows)
    (funcall (window%wrap-split-function 'split-window-horizontally))))

(defun window/split-vertically ()
  (interactive)
  (save-excursion
    (delete-other-windows)
    (funcall (window%wrap-split-function 'split-window-vertically))))

(defun window/delete-other-window ()
  (interactive)
  (if (equal (selected-window) popwin:popup-window)
      (popwin:one-window)
    (delete-other-windows)))

(global-set-key "\C-x|" 'window/split-horizontally)
(global-set-key "\C-x_" 'window/split-vertically)
(global-set-key "\C-x1" 'window/delete-other-window)

(defun window/toggle-window-split ()
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
      (setq new-window (split-window-vertically (round (* ratio (window-height))))
            align :horizontal))
     ((eq $align ?|)
      (setq new-window (split-window-horizontally (round (* ratio (window-width))))
            align :vertical))
     (t
      (message "Nothing to do !!!")))
    (when align
      (window%split-n orig-window $num1 align)
      (window%split-n new-window $num2 align))))

(provide 'init-windows)
