(defsubst mode-line%window-number ()
  (if (bound-and-true-p window-numbering-mode)
      (let* ((narrow-p (buffer-narrowed-p))
             (num-str (concat
                       (if narrow-p "<" " ")
                       (ignore-errors (window-numbering-get-number-string))
                       (if narrow-p ">" " "))))
        (add-face-text-property 0 (length num-str)
                                '(:background "black")
                                nil num-str)
        num-str)
    ""))

(defsubst mode-line%buffer-id ()
  "Display buffer id in mode-line."
  (let* ((method (file-remote-p default-directory 'method)))
    (list " %["
          (when method
            (propertize (concat "[" method "]") 'face font-lock-string-face))
          '(:propertize mode-line-buffer-identification
                        face font-lock-keyword-face)
          "%] ")))

(defsubst mode-line%buffer-major-mode ()
  "Display buffer major mode in mode-line."
  '(:propertize mode-name face font-lock-builtin-face))

(defsubst mode-line%buffer-status ()
  "Display buffer status.
Whether it is temporary file, whether it is modified, whether is read-only,
and `buffer-file-coding-system'"
  (list " ("
        (when (bound-and-true-p iedit-mode)
          (propertize (format "Iedit:%d " (iedit-counter))
                      'face font-lock-constant-face))
        (when (bound-and-true-p multiple-cursors-mode)
          (propertize (format "Mc:%d " (mc/num-cursors))
                      'face font-lock-constant-face))
        (when (and (boundp 'text-scale-mode-amount)
                   (/= text-scale-mode-amount 0))
          (propertize (format "%+d " text-scale-mode-amount)
                      'face font-lock-doc-face))
        (when (or defining-kbd-macro executing-kbd-macro)
          (propertize "Macro "
                      'face font-lock-variable-name-face))
        (when (buffer-temporary?)
          (propertize "Tmp " 'face font-lock-comment-face))
        (when (buffer-modified-p)
          (propertize "Mod " 'face font-lock-negation-char-face))

        (when buffer-read-only
          (propertize (if (bound-and-true-p view-mode) "View " "Ro ")
                      'face font-lock-string-face))

        (when (eq major-mode 'image-mode)
          (cl-destructuring-bind (width . height)
              (image-size (image-get-display-property) :pixels)
            (format "%dx%d " width height)))
        (let ((buf-coding (format "%s" buffer-file-coding-system)))
          (capitalize (if (string-match "\\(dos\\|unix\\|mac\\)" buf-coding)
                          (match-string 1 buf-coding)
                        buf-coding)))
        ")"))

(defsubst mode-line%flycheck ()
  "Display flycheck status in mode-line."
  (if (bound-and-true-p flycheck-mode)
      (list
       (pcase flycheck-last-status-change
         (`not-checked (propertize "Waiting" 'face 'font-lock-comment-face))
         (`no-checker  (propertize "No" 'face 'font-lock-comment-face))
         (`running (propertize "Running" 'face  'font-lock-doc-face))
         (`errored (propertize "Error" 'face  'flycheck-fringe-error))
         (`interrupted (propertize "Interrupted"
                                   'face  'flycheck-fringe-warning))
         (`suspicious (propertize "???" 'face 'flycheck-fringe-error))
         (`finished (let-alist (flycheck-count-errors flycheck-current-errors)
                      (concat (propertize (format "E%s" (or .error 0))
                                          'face 'flycheck-fringe-error)
                              " "
                              (propertize (format "W%s" (or .warning 0))
                                          'face 'flycheck-fringe-warning)))))
       " ")))

(defsubst mode-line%process ()
  "Display buffer process status."
  (if (get-buffer-process (current-buffer))
      (list "{" mode-line-process "} ")
    ""))

(defsubst mode-line%tail ()
  (propertize
   (if (buffer-file-name)
       "[L%l C%c %p %I]"
     "[L%l C%c %p]")
   'face 'font-lock-constant-face))

(defvar mode-line--center-margin 1)
(defvar mode-line-active? nil)
(defvar mode-line-default-format '("%e" (:eval (mode-line%generate))))
(defvar mode-line-config-alist
  '(((not mode-line-active?)
     (mode-line%window-number
      mode-line%buffer-id
      mode-line%buffer-major-mode)
     (mode-line%process)
     :no-center)
    ((or (memq major-mode '(dired-mode))
         (and (not (derived-mode-p 'text-mode 'prog-mode))
              (string-match-p "^\\*" (buffer-name))))
     (mode-line%window-number
      mode-line%buffer-id
      mode-line%buffer-major-mode
      mode-line%buffer-status)
     (mode-line%process)
     :no-center)
    (t (mode-line%window-number
        mode-line%buffer-id
        mode-line%buffer-major-mode
        mode-line%buffer-status)
       (mode-line%flycheck
        mode-line%process))))

(defun mode-line%format-line ($lhs $chs $rhs $tail)
  (let* ((lw (string-width $lhs))
         (cw (string-width $chs))
         (rw (+ (string-width $rhs) (length (format-mode-line $tail))))
         (tw (window-total-width))
         (margin (/ (- tw (+ lw rw cw)) 2)))
    (setq $chs
          (if (>= margin mode-line--center-margin)
              (list (make-string margin ?\s)
                    $chs
                    (make-string (max 0 (- tw (+ lw cw rw margin))) ?\s))
            (make-string (max mode-line--center-margin
                              (- tw (+ lw rw)))
                         ?\s)))
    (list $lhs $chs $rhs $tail)))

(defun mode-line%generate-body ($body)
  (let ((lhs (car $body))
        (rhs (cadr $body))
        (no-tail? (memq :no-tail $body))
        (no-center? (memq :no-center $body)))
    `(mode-line%format-line
      (format-mode-line (list ,@(mapcar #'list lhs)))
      ,(if no-center? "" '(format-mode-line mode-line-misc-info))
      (format-mode-line (list ,@(mapcar #'list rhs)))
      ,(if no-tail? "" '(format-mode-line (mode-line%tail))))))

(defmacro mode-line%compile ()
  `(byte-compile
    (defun mode-line%generate ()
      "Generate mode-line."
      (setq mode-line-active? (equal (selected-window)
                                     mode-line--current-window))
      (unless (bound-and-true-p eldoc-mode-line-string)
        (cond ,@(mapcar (lambda (config)
                          (list (car config)
                                (mode-line%generate-body (cdr config))))
                        mode-line-config-alist))))))

(mode-line%compile)

(defvar mode-line--current-window (frame-selected-window))
(defun mode-line*set-selected-window (&rest _)
  "Sets `mode-line--current-window' appropriately"
  (let ((win (frame-selected-window)))
    (unless (minibuffer-window-active-p win)
      (setq mode-line--current-window win))))
(advice-add #'select-window :after #'mode-line*set-selected-window)

;; Setup `mode-line-format'
(define-hook! mode-line|after-init-hook (after-init-hook)
  (window-numbering-mode 1)
  (setq-default mode-line-format mode-line-default-format)
  (setq-default mode-line-buffer-identification '("%b"))
  (setq-default mode-line-misc-info
                '((vc-mode vc-mode)
                  (company-mode company-lighter))))

(provide 'core-mode-line)

