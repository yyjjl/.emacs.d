;;  -*- lexical-binding: t -*-

(defsubst ymacs-popups//clean-window-list ()
  ;; Remove inactive window
  (setq ymacs-popups--window-list
        (--filter (window-live-p (car it)) ymacs-popups--window-list)))

(defun ymacs-popups//push-window (-window -buffer &optional -autoclose-p -action)
  (when -autoclose-p
    ;; Autoclose window should be dedicated
    (set-window-dedicated-p -window t)
    ;; Add to autoclose list
    (push (cons -window (or -action 'delete)) ymacs-popups--window-list))
  (with-current-buffer -buffer
    ;; Record popup window
    (setq ymacs-popups-current-window -window)))

(defun ymacs-popups//comint-buffer-p (-buffer)
  (let ((case-fold-search t))
    (with-current-buffer -buffer
      (or (memq major-mode ymacs-popups-comint-modes)
          (derived-mode-p 'comint-mode)
          (string-match-p ymacs-popups-comint-buffer-regexp (buffer-name))))))

(defun ymacs-popups//help-buffer-p (-buffer)
  (let ((case-fold-search t))
    (with-current-buffer -buffer
      (and (not (string-prefix-p "ivy-occur" (symbol-name major-mode)))
           (or (apply #'derived-mode-p ymacs-popups-help-modes)
               (string-match-p ymacs-popups-help-buffer-regexp (buffer-name)))))))
