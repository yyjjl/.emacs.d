(defun mode-line%window-number ()
  (if (bound-and-true-p window-numbering-mode)
      (let* ((narrow-p (buffer-narrowed-p))
             (num-str (concat
                       (if narrow-p "<" " ")
                       (ignore-errors (window-numbering-get-number-string))
                       (if narrow-p ">" " "))))
        (add-face-text-property 0 (length num-str)
                                '(:background "black"
                                              :box (:line-width 5 :color "black"))
                                nil num-str)
        num-str)
    ""))

(defun mode-line%buffer-id ()
  "Display buffer id in mode-line."
  (let* ((method (file-remote-p default-directory 'method)))
    (list " %["
          (when method
            (propertize (concat "[" method "]") 'face font-lock-string-face))
          '(:propertize mode-line-buffer-identification
                        face font-lock-keyword-face)
          "%] ")))

(defun mode-line%buffer-major-mode ()
  "Display buffer major mode in mode-line."
  '(:propertize mode-name face font-lock-builtin-face))

(defun mode-line%buffer-status ()
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

(defun mode-line%flycheck ()
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

(defun mode-line%vc ()
  "Display `version-control' status."
  (and (buffer-file-name (current-buffer))
       (concat vc-mode " ")))

(defun mode-line%process ()
  "Display buffer process status."
  (let ((proc (get-buffer-process (current-buffer))))
    (if proc (list "{" mode-line-process "} ")
      "")))

(defun mode-line%tail ()
  (if (buffer-file-name) "[L%l C%c %p %I]" "[L%l C%c %p]"))

(defvar mode-line--center-margin 1)
(defvar mode-line-active? nil)
(defvar mode-line-default-format '("%e" (:eval (mode-line%generate))))
(defvar mode-line-config-alist
  '((mode-line%inactive? (mode-line%window-number
                          mode-line%buffer-id
                          mode-line%buffer-major-mode)
                         (mode-line%process)
                         nil
                         :no-center)
    (mode-line%use-special? (mode-line%window-number
                             mode-line%buffer-id
                             mode-line%buffer-major-mode
                             mode-line%buffer-status)
                            (mode-line%process)
                            nil
                            :no-center)
    (t (mode-line%window-number
        mode-line%buffer-id
        mode-line%buffer-major-mode
        mode-line%buffer-status)
       (mode-line%flycheck
        mode-line%process
        mode-line%vc))))
(defun mode-line%inactive? ($bn)
  (not mode-line-active?))

(defun mode-line%use-special? ($bn)
  (or (memq major-mode '(dired-mode))
      (and (not (derived-mode-p 'text-mode 'prog-mode))
           (string-match-p "^\\*" $bn))))

(defun mode-line%create ($left $right &optional $no-tail? $no-center?)
  (let* ((lhs (format-mode-line (mapcar #'funcall $left)))
         (chs (if $no-center? ""
                (propertize (format-mode-line mode-line-misc-info)
                            'face font-lock-string-face)))
         (rhs (format-mode-line (mapcar #'funcall $right)))
         (tail (if $no-tail? ""
                 (propertize (mode-line%tail) 'face 'font-lock-constant-face)))
         (lw (string-width lhs))
         (cw (string-width chs))
         (rw (+ (string-width rhs) (length (format-mode-line tail))))
         (tw (window-total-width))
         (margin (/ (- tw (+ lw rw cw)) 2)))
    (setq chs
          (if (>= margin mode-line--center-margin)
              (list (make-string margin ?\s)
                    chs
                    (make-string (max 0 (- tw (+ lw cw rw margin))) ?\s))
            (make-string (max mode-line--center-margin
                              (- tw (+ lw rw)))
                         ?\s)))
    (list lhs chs rhs tail)))

(defun mode-line%generate ()
  "Generate mode-line."
  (setq mode-line-active?
        (equal (selected-window) mode-line--current-window))
  (unless (bound-and-true-p eldoc-mode-line-string)
    (let ((config-alist mode-line-config-alist)
          config
          mode-line)
      (while (and config-alist
                  (setq config (pop config-alist)))
        (when (or (eq (car config) t)
                  (funcall (car config) (buffer-name)))
          (setq config-alist nil)
          (setq mode-line (apply #'mode-line%create (cdr config)))))
      mode-line)))

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
                '((global-mode-string ("" global-mode-string " "))
                  (projectile-mode ("" projectile-mode-line " ")))))

(provide 'core-mode-line)
