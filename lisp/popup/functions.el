;;  -*- lexical-binding: t -*-

(defsubst ymacs-popup//get-current-window (&optional -frame)
  (let ((window (frame-parameter -frame 'ymacs-popup-window)))
    (and (window-live-p window)
         window)))

(defsubst ymacs-popup//get-current-buffer (&optional -frame)
  (let ((buffer (frame-parameter -frame 'ymacs-popup-buffer)))
    (and (buffer-live-p buffer)
         buffer)))

(defsubst ymacs-popup//get-term-window (&optional -frame)
  (let ((window (frame-parameter -frame 'ymacs-term-window)))
    (and (window-live-p window)
         (ymacs-popup//term-buffer-p (window-buffer window))
         window)))

(defsubst ymacs-popup//push-window (-window -buffer &optional -autoclose-p)
  (when -autoclose-p
    ;; Autoclose window should be dedicated
    (set-window-dedicated-p -window t)
    ;; Add to autoclose list
    (setq ymacs-popup--window-list
          (cons -window (cl-remove -window ymacs-popup--window-list))))

  (set-frame-parameter nil 'ymacs-popup-window -window)
  (set-frame-parameter nil 'ymacs-popup-buffer -buffer))

(defsubst ymacs-popup//set-term-window (-window &optional -frame -move-to-head)
  (set-window-dedicated-p -window t)

  (set-frame-parameter -frame 'ymacs-term-window -window)

  (let ((buffer (window-buffer -window)))
    (set-frame-parameter nil 'ymacs-popup-buffer buffer)
    (if -move-to-head
        (setq ymacs-popup--term-buffer-list
              (cons buffer
                    (cl-remove buffer ymacs-popup--term-buffer-list)))
      (cl-pushnew buffer ymacs-popup--term-buffer-list))))

(defsubst ymacs-popup//cleanup ()
  ;; Remove inactive window and buffer
  (setq ymacs-popup--window-list
        (cl-remove-if-not #'window-live-p ymacs-popup--window-list))
  (setq ymacs-popup--term-buffer-list
        (cl-remove-if-not
         (lambda (buffer)
           (and (buffer-live-p buffer)
                (ymacs-popup//term-buffer-p buffer)))
         ymacs-popup--term-buffer-list)))

(defsubst ymacs-popup//term-buffer-p (-buffer)
  (let ((case-fold-search t))
    (with-current-buffer -buffer
      (and (not (string-prefix-p "ivy-occur" (symbol-name major-mode)))
           (or
            (apply #'derived-mode-p ymacs-popup-term-modes)
            (string-match-p ymacs-popup-term-buffer-regexp (buffer-name)))))))

(defsubst ymacs-popup//help-buffer-p (-buffer)
  (let ((case-fold-search t))
    (with-current-buffer -buffer
      (and (not (string-prefix-p "ivy-occur" (symbol-name major-mode)))
           (or (apply #'derived-mode-p ymacs-popup-help-modes)
               (string-match-p ymacs-popup-help-buffer-regexp (buffer-name)))))))

(defsubst ymacs-popup//get-term-buffer-list ()
  (ymacs-popup//cleanup)
  ymacs-popup--term-buffer-list)

(defun ymacs-popup//rule-to-form (rule)
  (-let* (((macth-fn . rule) (plist-pop! (copy-sequence rule) :macth-fn))
          ((name . rule) (plist-pop! rule :name))
          ((mode . rule) (plist-pop! rule :mode))
          ((name-regexp . rule) (plist-pop! rule :name-regexp)))
    (list (cond
           (macth-fn `(,macth-fn buffer))
           (name (if (stringp name)
                     `(string= ,name buffer-name)
                   `(member buffer-name ',name)))
           (mode (if (symbolp mode)
                     `(eq ',mode buffer-major-mode)
                   `(memq buffer-major-mode ',mode)))
           (name-regexp `(string-match-p ,name-regexp buffer-name)))
          `(with-current-buffer buffer
             (setq ymacs-popup--matched-rule ',rule)))))

(defmacro ymacs-popup//compile-matcher ()
  "transform `ymacs-popup-rules' into a matcher function"
  `(defun ymacs-popup//match (-buffer-or-name _alist)
     (let* ((buffer (get-buffer -buffer-or-name))
            (buffer-major-mode (buffer-local-value 'major-mode buffer))
            (buffer-name (buffer-name buffer)))
       (cond ,@(mapcar #'ymacs-popup//rule-to-form ymacs-popup-rules)))))

(defun ymacs-popup//display-buffer (-buffer -alist -rule)
  "Internal function for `ymacs-popup/display-buffer-action'.
Displays -BUFFER according to -ALIST and -RULE."
  (let ((display-fn (plist-get -rule :display-fn))
        (side (plist-get -rule :side)))
    (cond
     ;; custom display-fn
     (display-fn
      (let ((window (funcall display-fn -buffer -alist -rule)) _)
        (unless (window-live-p window)
          (user-error "display-fn didn't return window: %S %S" window display-fn))))
     ;; ignore buffer
     ((plist-get -rule :ignore) 'fail)
     ;; reuse window
     ((display-buffer-reuse-window -buffer -alist))
     ;; side window
     (side
      (display-buffer-in-direction
       -buffer
       `((window . main)
         (window-parameters . ((ymacs-quit-action . delete)))
         (direction . ,side)
         (window-height . ,(or (plist-get -rule :size) ymacs-popup-default-size))
         ,@-alist)))
     ;; fallback
     ((display-buffer-pop-up-window -buffer -alist))
     ((display-buffer-use-some-window -buffer -alist)))))

(defun ymacs-popup//display-term-buffer (-buffer -alist -rule)
  (let ((window (ymacs-popup//get-term-window)))
    (if window
        ;; Reuse window
        (progn
          (select-window window)
          (set-window-dedicated-p window nil)
          (set-window-buffer window -buffer))

      (setq window
            (display-buffer-in-direction
             -buffer
             `((window . main)
               (direction . ,(or (plist-get -rule :size) ymacs-popup-default-side))
               (window-height . ,(or (plist-get -rule :size) ymacs-popup-default-size))
               ,@-alist))))
    ;; move term buffer to the head of term-buffer-list
    (ymacs-popup//set-term-window window nil t)
    window))

(defun ymacs-popup//display-buffer-action (-buffer -alist)
  "Display -BUFFER-OR-NAME according to the result of `ymacs-popup//match'"
  (ymacs-popup//cleanup)

  (let* ((ignore-window-parameters t)
         (rule (buffer-local-value 'ymacs-popup--matched-rule -buffer))
         (window
          (funcall (if (plist-get rule :terminal)
                       #'ymacs-popup//display-term-buffer
                     #'ymacs-popup//display-buffer)
                   -buffer -alist rule)))
    (when (window-live-p window)
      (when (plist-get rule :dedicated)
        (set-window-dedicated-p window t))

      (when (plist-get rule :autoclose)
        (ymacs-popup//push-window window -buffer t))

      (when (plist-get rule :select)
        (select-window window)))
    window))

(ymacs-popup//compile-matcher)
