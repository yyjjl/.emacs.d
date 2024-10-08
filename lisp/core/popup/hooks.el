;;  -*- lexical-binding: t -*-

(declare-function winner-ring "winner")
(declare-function winner-undo "winner")

(define-hook! (ymacs-popup//frame-setup &optional -frame)
  (window-setup-hook ;; when setup
   after-make-frame-functions)
  (set-frame-parameter -frame 'buffer-predicate #'ymacs-popup//buffer-predicate))

(define-hook! ymacs-popup//autoclose-popup-window (kill-buffer-hook)
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
  (when (active-minibuffer-window)
    (ymacs-editor//display-help--hide))
  (when (and (called-interactively-p 'interactive)
             (not (region-active-p)))
    (let (window)
      (if (and (one-window-p)
               (bound-and-true-p winner-mode))
          (when (or (eq (selected-window)
                        (ymacs-popup//get-current-window))
                    (and (eq (selected-window)
                             (ymacs-popup//get-term-window))
                         (> (length
                             (cdr (ring-ref
                                   (winner-ring (selected-frame))
                                   1)))
                            1)))
            (winner-undo))
        (setq window (car ymacs-popup--window-list))
        (when (and (window-live-p window)
                   (with-current-buffer (window-buffer window)
                     (plist-get ymacs-popup--matched-rule :autoclose)))
          (if (window-parameter window 'quite-restore)
              ;; try restore
              (quit-window nil window)
            (delete-window window))
          (pop ymacs-popup--window-list))))))

(advice-add 'keyboard-quit :before #'keyboard-quit@autoclose)
(advice-add 'other-window :before #'keyboard-quit@autoclose)

;; 一些 other window 函数不能切割 popup window
(define-advice window--try-to-split-window (:around (-fn -window &optional -alist) dont-split-popups)
  (when (or (one-window-p)
            (not (eq -window
                     (when-let (buffer (window-buffer -window))
                       (buffer-local-value 'ymacs-popup--nosplit-window buffer)))))
    (funcall -fn -window -alist)))

(run-after-init! 100
  (setq display-buffer-alist
        '((ymacs-popup//match ymacs-popup//display-buffer-action))))
