;;; -*- lexical-binding: t; -*-

(defvar ymacs-editor-window-size-list '(0.25 0.382 0.5 0.618 0.7))
(defun ymacs-editor//window-split-with-size (-size &optional -vertical)
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
(defun ymacs-editor/window-split-vertically (&optional -arg)
  (interactive "P")
  (ymacs-editor/window-split-horizontally -arg t))

;;;###autoload
(defun ymacs-editor/window-split-horizontally (&optional -arg -vertical)
  (interactive "P")
  (when (eq (selected-window)
            (ymacs-popup//get-term-window))
    (user-error "Terminal window can not split"))
  (if -arg
      (ymacs-editor//window-split-with-size
       (completing-read! (concat "Split " (if -vertical "Vertically" "Horizontally"))
                         ymacs-editor-window-size-list
                         (lambda ()
                           (ymacs-editor//window-split-with-size (read-number "Float or Integer: ") -vertical))
                         "Read number")
       -vertical)
    (ymacs-editor//window-split-with-size nil -vertical)))

;; Rearrange split windows
;;;###autoload
(defun ymacs-editor/window-force-split-horizontally (&optional -arg)
  (interactive "P")
  (save-excursion
    (delete-other-windows)
    (ymacs-editor/window-split-horizontally -arg)))

;;;###autoload
(defun ymacs-editor/window-force-split-vertically (&optional -arg)
  (interactive "P")
  (save-excursion
    (delete-other-windows)
    (ymacs-editor/window-split-vertically -arg)))

;;;###autoload
(defun ymacs-editor/aw-select-window ()
  "Select the specified window."
  (interactive)
  (let ((path (-> (this-command-keys-vector)
                  key-description
                  (split-string "-")
                  (elt 1))))
    (unless (cl-loop
             for window in (aw-window-list)
             when (and (window-live-p window)
                       (equal path (window-parameter window 'ace-window-path)))
             return (aw-switch-to-window window))
      (message "No specified window: %s" path))))

;;;###autoload
(defun ymacs-editor/toggle-aw-scope ()
  (interactive)
  (setq aw-scope
        (completing-read! "Scope: " (remove aw-scope '(visible global frame))))
  (aw-update)
  (force-mode-line-update t)
  (message "Current AW scope: %s" (upcase (symbol-name aw-scope))))
