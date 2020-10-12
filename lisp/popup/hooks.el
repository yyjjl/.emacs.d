;;  -*- lexical-binding: t -*-

(define-hook! ymacs-popup|after-init (after-init-hook)
  (setq display-buffer-alist
        '((ymacs-popup//match ymacs-popup//display-buffer-action))))

(define-hook! (ymacs-popup|compilation-finish-hook -buffer _)
  (compilation-finish-functions)
  (when (buffer-live-p -buffer)
    (with-current-buffer -buffer
      (unless (eq major-mode 'compilation-mode)
        ;; Sometime it will open a comint buffer
        (compilation-mode)
        (when-let (window (get-buffer-window -buffer))
          (ymacs-popup//push-window window -buffer t))))))

(define-hook! ymacs-popup|autoclose-popup-window (kill-buffer-hook)
  "Auto quit popup window after buffer killed"
  (let ((window (get-buffer-window)))
    (when (and window
               (memq window ymacs-popup--window-list)
               (not (one-window-p))
               (not (minibuffer-window-active-p window))
               (or (not (boundp 'lv-wnd))
                   (not (eq (next-window) lv-wnd))))
      (quit-window nil window))))

(defun keyboard-quit@autoclose (&rest _)
  "When `C-g' pressed, close latest opened popup window"
  (ymacs-popup//cleanup)
  (when (and (called-interactively-p 'interactive)
             (not (region-active-p)))
    (let (window)
      (if (one-window-p)
          (when (eq (selected-window)
                    (ymacs-popup//get-current-window))
            (winner-undo))
        (setq window (pop ymacs-popup--window-list))
        (when (window-live-p window)
          (if (eq (window-parameter window 'ymacs-quit-action) 'delete)
              (delete-window window)
            (quit-window nil window)))))))

(advice-add 'keyboard-quit :before #'keyboard-quit@autoclose)
(advice-add 'other-window :before #'keyboard-quit@autoclose)

(define-advice window--try-to-split-window (:around (-fn -window &optional -alist) dont-split-popups)
  (unless (or (memq -window ymacs-popup--window-list)
              (eq -window (ymacs-popup//get-term-window)))
    (funcall -fn -window -alist)))
