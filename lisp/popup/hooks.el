;;  -*- lexical-binding: t -*-

(define-hook! shackle|autoclose-popup-window (kill-buffer-hook)
  "Auto quit popup window after buffer killed"
  (let ((window (get-buffer-window)))
    (when (and window
               (equal window ymacs-popups-current-window)
               (not (one-window-p))
               (not (minibuffer-window-active-p window))
               (or (not (boundp 'lv-wnd))
                   (not (eq (next-window) lv-wnd))))
      (quit-window nil window))))

(defun keyboard-quit@autoclose (&rest _)
  "When `C-g' pressed, close latest opened popup window"
  (ymacs-popups//clean-window-list)
  (when (and (called-interactively-p 'interactive)
             (not (region-active-p)))
    (let (window
          action)
      (if (one-window-p)
          (progn
            (setq window (selected-window))
            (when (equal (buffer-local-value 'ymacs-popups-current-window (window-buffer window))
                         window)
              (winner-undo)))
        (setq window (car (car ymacs-popups--window-list)))
        (setq action (cdr (car ymacs-popups--window-list)))
        (when (window-live-p window)
          (if (eq action 'quit)
              (quit-window nil window)
            (delete-window window))
          (pop ymacs-popups--window-list))))))

(advice-add 'keyboard-quit :before #'keyboard-quit@autoclose)
(advice-add 'other-window :before #'keyboard-quit@autoclose)

(define-advice window--try-to-split-window (:around (-fn -window &optional -alist) skip-popups)
  (let ((buffer (and (window-live-p -window)
                     (window-buffer -window))))
    (unless (and buffer
                 (not (one-window-p))
                 (window-live-p (buffer-local-value 'ymacs-popups-current-window buffer)))
      (funcall -fn -window -alist))))


(after! shackle
  (define-advice shackle-display-buffer (:around (-fn -buffer -alist -plist) handle-autoclose)
    (ymacs-popups//clean-window-list)
    (setq shackle-default-alignment
          (if (> (frame-width)
                 (or split-width-threshold 120))
              'right
            'below))
    (let* ((autoclose-p (plist-get -plist :autoclose))
           (dedicated-p (plist-get -plist :dedicated))
           (old-window (get-buffer-window -buffer))
           (action 'delete)
           (window (or (funcall -fn -buffer -alist -plist)
                       (progn
                         (setq action 'quit)
                         (funcall -fn -buffer -alist (cl-list* :other t -plist))))))
      (unless (eq window old-window)
        (when dedicated-p
          (set-window-dedicated-p window t))
        (ymacs-popups//push-window window -buffer autoclose-p action))
      window))

  (define-advice shackle--display-buffer-popup-window (:override (-buffer -alist -plist) reuse)
    (let ((frame (shackle--splittable-frame))
          (other-window-p (and (plist-get -plist :other)
                               (not (one-window-p)))))
      (when frame
        (let ((window (if other-window-p
                          (next-window nil 'nominibuf)
                        (shackle--split-some-window frame -alist)))
              (type (if other-window-p 'reuse 'window)))
          (prog1
              (shackle--window-display-buffer -buffer window type -alist)
            (when window
              (setq shackle-last-window window)
              (setq shackle-last-buffer -buffer))
            (unless (cdr (assq 'inhibit-switch-frame -alist))
              (window--maybe-raise-frame (window-frame window)))))))))
