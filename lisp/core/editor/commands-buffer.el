;;; -*- lexical-binding: t; -*-

;;;###autoload
(defun ymacs-editor/create-scratch-buffer (&optional -major-mode)
  "Create a new scratch buffer to work in. (could be *scratch* - *scratchX*)."
  (interactive
   (list
    (if current-prefix-arg
        (intern
         (completing-read
          "Select Mode: " obarray
          (lambda (sym)
            (and (fboundp sym)
                 (string-suffix-p "-mode" (symbol-name sym))))
          :require-match))
      major-mode)))
  (let ((buffer (get-buffer-create
                 (cl-loop
                  for n from 0
                  for name = (format "*scratch%s*" (if (= n 0) "" n))
                  unless (get-buffer name)
                  return name))))
    (with-current-buffer buffer
      (funcall -major-mode))
    (pop-to-buffer buffer)))

(defun ymacs-editor//move-buffer (-direction)
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
(defun ymacs-editor/move-buffer ()
  "Helper function to move the current buffer to the window in the given
   -direction (with must be 'up, 'down', 'left or 'right). An error is
   thrown, if no window exists in this direction."
  (interactive)

  (require 'windmove)
  (lv-message "use h,j,k,l,<left>,<right>,<up>,<down> to move")
  (cl-letf ((move-left (interactive! (ymacs-editor//move-buffer 'left)))
            (move-right (interactive! (ymacs-editor//move-buffer 'right)))
            (move-down (interactive! (ymacs-editor//move-buffer 'down)))
            (move-up (interactive! (ymacs-editor//move-buffer 'up))))
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
