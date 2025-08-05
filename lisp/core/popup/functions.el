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
    (setq ymacs-popup--matched-rule
          (plist-put (copy-sequence ymacs-popup--matched-rule) :autoclose t))
    (setq ymacs-popup--window-list (cons -window ymacs-popup--window-list)))

  (set-frame-parameter nil 'ymacs-popup-window -window)
  (set-frame-parameter nil 'ymacs-popup-buffer -buffer))

(defsubst ymacs-popup//set-term-window (-window &optional -frame -move-to-head)
  (set-window-dedicated-p -window t)

  (set-frame-parameter -frame 'ymacs-term-window -window)

  (let ((buffer (window-buffer -window)))
    (set-frame-parameter nil 'ymacs-popup-buffer buffer)
    (if -move-to-head
        (setq ymacs-popup--term-buffer-list
              `(,@(cl-remove buffer ymacs-popup--term-buffer-list) ,buffer))
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
      (and (not (string-prefix-p "*Embark Export" (buffer-name)))
           (not (derived-mode-p 'gud-mode))
           (or
            (apply #'derived-mode-p ymacs-popup-term-modes)
            (string-match-p ymacs-popup-term-buffer-regexp (buffer-name)))))))

(defsubst ymacs-popup//help-buffer-p (-buffer)
  (let ((case-fold-search t))
    (with-current-buffer -buffer
      (and (not (string-prefix-p "*Embark Export" (buffer-name)))
           (or (apply #'derived-mode-p ymacs-popup-help-modes)
               (string-match-p ymacs-popup-help-buffer-regexp (buffer-name)))))))

(defsubst ymacs-popup//occur-buffer-p (-buffer)
  (let ((case-fold-search t))
    (with-current-buffer -buffer
      (or (string-prefix-p "*Embark Export" (buffer-name))
          (derived-mode-p 'occur-mode 'embark-collect-mode)
          (string-match-p ymacs-popup-occur-buffer-regexp (buffer-name))))))

(defsubst ymacs-popup//get-term-buffer-list ()
  (ymacs-popup//cleanup)
  ymacs-popup--term-buffer-list)

(defsubst ymacs-popup//get-active-term-buffer-list ()
  (ymacs-popup//cleanup)
  (cl-remove-if-not
   (lambda (buffer) (process-live-p (get-buffer-process buffer)))
   ymacs-popup--term-buffer-list))

(defsubst ymacs-popup//get-side-window-size (-side -size)
  (unless (eq -size 'auto)
    (list (cons (if (memq -side '(below above up down top bottom))
                    'window-height
                  'window-width)
                (or -size ymacs-popup-default-size)))))

(defsubst ymacs-popup//display-buffer-in-side-window (-buffer -alist -side -size -terminal-p)
  (let* ((windows
          (cl-loop for w in (window-list)
                   when (eq (window-parameter w 'window-side) -side)
                   collect w))
         (slots (mapcar (lambda (w) (window-parameter w 'window-slot)) windows)))
    (or (when -terminal-p
          (when-let (term-window (ymacs-popup//get-term-window))
            ;; Case 1: reuse term-window
            (set-window-dedicated-p term-window nil)
            (set-window-buffer term-window -buffer)
            term-window))
        (let* ((min-slot (if slots (apply #'min slots) 1))
               (slot (cond
                      (-terminal-p ymacs-popup-max-slots)
                      ;; Case 2: the left/up-most slot window
                      ((>= (length windows) ymacs-popup-max-slots) min-slot)
                      ;; Case 3: left/up of all slot windows
                      (t (1- min-slot)))))
          (display-buffer-in-side-window
           -buffer
           `(;; (window . main)
             (side . ,-side)
             (slot . ,slot)
             ;; (direction . ,side)
             ,@(ymacs-popup//get-side-window-size -side -size)
             ,@-alist))))))

(defsubst ymacs-popup//display-buffer (-buffer -alist -rule)
  "Internal function for `ymacs-popup/display-buffer-action'.
Displays -BUFFER according to -ALIST and -RULE."
  (let ((display-fn (plist-get -rule :display-fn))
        (side (plist-get -rule :side))
        (size (plist-get -rule :size))
        (terminal-p (plist-get -rule :terminal)))
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
     (side (ymacs-popup//display-buffer-in-side-window -buffer -alist side size terminal-p))
     ;; fallback
     ((display-buffer-pop-up-window -buffer -alist))
     ((display-buffer-use-some-window -buffer -alist)))))

(defsubst ymacs-popup//display-buffer-action (-buffer -alist)
  "Display -BUFFER-OR-NAME according to the result of `ymacs-popup//match'"
  (ymacs-popup//cleanup)

  (let* ((ignore-window-parameters t)
         (rule (buffer-local-value 'ymacs-popup--matched-rule -buffer))
         (window (ymacs-popup//display-buffer -buffer -alist rule)))

    (when (window-live-p window)
      (cond
       ((plist-get rule :dedicated)
        (set-window-dedicated-p window t))

       ((plist-get rule :autoclose)
        (ymacs-popup//push-window window -buffer t))

       ((plist-get rule :terminal)
        (ymacs-popup//set-term-window window nil t)))

      (with-current-buffer -buffer
        (when (plist-get rule :no-mode-line)
          (setq-local mode-line-format nil))
        (when (plist-get rule :no-tab-line)
          (setq-local tab-line-format nil))
        (when (window-dedicated-p window)
          (setq ymacs-popup--nosplit-window window)))

      (when (plist-get rule :select)
        (select-window window)))
    window))

(defun ymacs-popup//buffer-predicate (-buffer)
  (or (buffer-file-name -buffer)
      (not
       (or
        (eq (buffer-local-value 'major-mode -buffer) 'fundamental-mode)
        (when-let (rule (buffer-local-value 'ymacs-popup--matched-rule -buffer))
          (or (plist-get rule :dedicated)
              (plist-get rule :autoclose)
              (plist-get rule :terminal)))
        (ymacs-popup//help-buffer-p -buffer)
        (ymacs-popup//occur-buffer-p -buffer)
        (ymacs-popup//term-buffer-p -buffer)))))



(defsubst ymacs-popup//rule-to-form (rule)
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
          `',rule)))

(defmacro ymacs-popup//compile-matcher (&rest -popup-rules)
  "transform `-popup-rules' into a matcher function"
  `(progn
     (defsubst ymacs-popup//match (-buffer-or-name _alist)
       (let* ((buffer (get-buffer -buffer-or-name))
              ,@(when (cl-some
                       (lambda (rule)
                         (plist-get rule :mode))
                       -popup-rules)
                  '((buffer-major-mode (buffer-local-value 'major-mode buffer))))
              ,@(when (cl-some
                       (lambda (rule)
                         (or (plist-get rule :name)
                             (plist-get rule :name-regexp)))
                       -popup-rules)
                  '((buffer-name (buffer-name buffer)))))
         (with-current-buffer buffer
           (setq ymacs-popup--matched-rule
                 (cond ,@(mapcar #'ymacs-popup//rule-to-form -popup-rules))))))
     ,(unless (bound-and-true-p byte-compile-current-file)
        `(let (byte-compile-warnings)
           (byte-compile #'ymacs-popup//match)))))

(ymacs-popup//compile-matcher
 (:macth-fn ymacs-popup//help-buffer-p
  :side bottom
  :select t
  :autoclose t)
 (:name "*interpretation*"
  :side right
  :select t
  :size 0.3
  :no-tab-line t
  :dedicated t)
 (:macth-fn ymacs-popup//term-buffer-p
  :side bottom
  :select t
  :terminal t)
 (:macth-fn ymacs-popup//occur-buffer-p
  :select t
  :side bottom
  :dedicated t)
 ;; (:name-regexp ymacs-popup-left-dedicated-buffer-regexp
 ;;  :select t
 ;;  :side left
 ;;  :size 60
 ;;  :dedicated t)
 (:name-regexp ymacs-popup-below-autoclose-buffer-regexp
  :size auto
  :side bottom
  ;; :no-mode-line t
  :autoclose t)
 (:name-regexp ymacs-popup-other-window-regexp
  :select t))
