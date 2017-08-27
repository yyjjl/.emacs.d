(defun mode-line%window-number ()
  (let* ((narrow-p (buffer-narrowed-p))
         (num-str (concat
                   (if narrow-p "<" " ")
                   (and (bound-and-true-p window-numbering-mode)
                        (ignore-errors (window-numbering-get-number-string)))
                   (if narrow-p ">" " "))))
    (add-face-text-property 0 (length num-str)
                            '(:inherit mode-line-window-number)
                            nil num-str)
    num-str))

(defun mode-line%buffer-id ()
  "Display buffer id in mode-line.
Default format 'window-number %[%b (host-address) %]'
If function `window-numbering-mode' enabled window-number will be showed.
If buffer file is a remote file, host-address will be showed"
  (let* ((host (and default-directory
                    (let ((tmp (file-remote-p default-directory)))
                      (and tmp (split-string tmp ":")))))
         (real-id (concat
                   (propertize
                    (format-mode-line  mode-line-buffer-identification)
                    'face 'font-lock-keyword-face)
                   (if (and host (cdr host))
                       (propertize (concat "(" (cadr host) ")")
                                   'face font-lock-string-face)
                     ""))))
    (list " %[" real-id "%] ")))

(defun mode-line%buffer-major-mode ()
  "Display buffer major mode in mode-line."
  (propertize "%m " 'face 'font-lock-builtin-face))

(defun mode-line%buffer-status ()
  "Display buffer status.
Whether it is temporary file, whether it is modified, whether is read-only,
and `buffer-file-coding-system'"
  (list "("
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
          (propertize "RO " 'face font-lock-string-face))
        (when (eq major-mode 'image-mode)
          (cl-destructuring-bind (width . height)
              (image-size (image-get-display-property) :pixels)
            (format "%dx%d " width height)))
        (let ((buf-coding (format "%s" buffer-file-coding-system)))
          (upcase (if (string-match "\\(dos\\|unix\\|mac\\)" buf-coding)
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
(defvar mode-line-default-format '("%e" (:eval (mode-line%generate))))
(defvar mode-line--default-config '((mode-line%window-number
                                     mode-line%buffer-id
                                     mode-line%buffer-major-mode
                                     mode-line%buffer-status)
                                    (mode-line%flycheck
                                     mode-line%process
                                     mode-line%vc)))
(defvar mode-line--speical-config '((mode-line%window-number
                                     mode-line%buffer-id
                                     mode-line%buffer-major-mode
                                     mode-line%buffer-status)
                                    (mode-line%process)))
(defvar mode-line--inactive-config '((mode-line%window-number
                                      mode-line%buffer-id
                                      mode-line%buffer-major-mode)
                                     (mode-line%process)))

(defun mode-line%create (left right)
  (let* ((lhs (format-mode-line (mapcar #'funcall left)))
         (chs (format-mode-line mode-line-misc-info))
         (rhs (format-mode-line (mapcar #'funcall right)))
         (tail (propertize (mode-line%tail) 'face 'font-lock-constant-face))
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
  (unless (bound-and-true-p eldoc-mode-line-string)
    (let ((bn (buffer-name)))
      (apply #'mode-line%create
             (cond ((or (not (equal (selected-window)
                                    mode-line--current-window)))
                    mode-line--inactive-config)
                   ((or (memq major-mode '(dired-mode
                                           lisp-interaction-mode))
                        (and (not (derived-mode-p 'text-mode 'prog-mode))
                             (string-match-p "^\\*" bn)))
                    mode-line--speical-config)
                   (t mode-line--default-config))))))

(defvar mode-line--current-window (frame-selected-window))
(defun mode-line*set-selected-window (&rest _)
  "Sets `+doom-modeline-current-window' appropriately"
  (let ((win (frame-selected-window)))
    (unless (minibuffer-window-active-p win)
      (setq mode-line--current-window win))))
(advice-add #'select-window :after #'mode-line*set-selected-window)


;; Setup `mode-line-format'
(define-hook! mode-line|after-init-hook (after-init-hook)
  (window-numbering-mode 1)
  (setq-default mode-line-format mode-line-default-format)
  (setq-default mode-line-buffer-identification '("%b"))
  (setq-default frame-title-format
                '(:eval (let ((fn (buffer-file-name)))
                          (if fn
                              (abbreviate-file-name fn)
                            (buffer-name))))))

(provide 'core-modeline)
