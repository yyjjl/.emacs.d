(defsubst mode-line//window-number ()
  (and (bound-and-true-p window-numbering-mode)
       (let ((narrow-p (buffer-narrowed-p)))
         (propertize (concat
                      (if narrow-p "<" " ")
                      (ignore-errors (window-numbering-get-number-string))
                      (if narrow-p ">" " "))
                     'face 'window-numbering-face))))

(defvar-local mode-line--cached-relative-directory nil)
(defsubst mode-line//relative-directory ()
  (or (and projectile-cached-buffer-file-name
           (equal projectile-cached-buffer-file-name (or buffer-file-name 'none))
           mode-line--cached-relative-directory)
      (setq mode-line--cached-relative-directory
            (let ((root (file-truename (projectile-project-root)))
                  (directory (file-truename default-directory)))
              (if (and (string-prefix-p root directory) (buffer-file-name))
                  (mapconcat
                   (lambda (x) (if (equal x "") "" (substring x 0 1)))
                   (split-string (substring directory (length root)) "/")
                   "/")
                "")))))

(defsubst mode-line//buffer-id ()
  "Display buffer id in mode-line."
  (let ((method (file-remote-p default-directory 'method)))
    (list " %["
          (propertize (if method
                          (concat "[" method "]")
                        (mode-line//relative-directory))
                      'face 'font-lock-string-face)
          '(:propertize mode-line-buffer-identification
                        face font-lock-keyword-face)
          "%] ")))

(defsubst mode-line//buffer-major-mode ()
  "Display buffer major mode in mode-line."
  '(:propertize mode-name face font-lock-builtin-face))

(defsubst mode-line//buffer-status ()
  "Display buffer status.

Whether it is temporary file, whether it is modified, whether is
read-only, and `buffer-file-coding-system'"
  (list " ("
        (when (bound-and-true-p iedit-mode)
          (propertize (format "IE:%d " (iedit-counter))
                      'face font-lock-constant-face))
        (when (bound-and-true-p multiple-cursors-mode)
          (propertize (format "MC:%d " (mc/num-cursors))
                      'face font-lock-constant-face))
        (when (and (boundp 'text-scale-mode-amount)
                   (/= text-scale-mode-amount 0))
          (propertize (format "%+d " text-scale-mode-amount)
                      'face font-lock-doc-face))
        (when (or defining-kbd-macro executing-kbd-macro)
          (propertize "Macro " 'face font-lock-variable-name-face))
        (when (buffer-temporary?)
          (propertize "Tmp " 'face font-lock-comment-face))
        (when (buffer-modified-p)
          (propertize "Mod " 'face font-lock-negation-char-face))
        (when (buffer-base-buffer) "I ")
        (when buffer-read-only
          (propertize "RO " 'face font-lock-string-face))
        (when visual-line-mode
          (propertize "V " 'face font-lock-type-face))
        (when (eq major-mode 'image-mode)
          (cl-destructuring-bind (width . height)
              (image-size (image-get-display-property) :pixels)
            (format "%dx%d " width height)))
        (let ((buffer-encoding (format "%s" buffer-file-coding-system)))
          (capitalize (if (string-match "\\(dos\\|unix\\|mac\\)" buffer-encoding)
                          (match-string 1 buffer-encoding)
                        buffer-encoding)))
        ")"))

(autoload 'image-get-display-property "image-mode" nil)
(defsubst mode-line//flycheck ()
  "Display flycheck status in mode-line."
  (and (bound-and-true-p flycheck-mode)
       (concat
        " "
        (pcase flycheck-last-status-change
          (`not-checked (propertize "Waiting" 'face 'font-lock-comment-face))
          (`no-checker (propertize "No" 'face 'font-lock-comment-face))
          (`running (propertize "Running" 'face 'font-lock-doc-face))
          (`errored (propertize "Error" 'face 'flycheck-fringe-error))
          (`interrupted (propertize "Interrupted" 'face 'flycheck-fringe-warning))
          (`suspicious (propertize "???" 'face 'flycheck-fringe-error))
          (`finished (let-alist (flycheck-count-errors flycheck-current-errors)
                       (concat (propertize (format "E%s" (or .error 0))
                                           'face 'flycheck-fringe-error)
                               " "
                               (propertize (format "W%s" (or .warning 0))
                                           'face 'flycheck-fringe-warning))))))))

(defsubst mode-line//process ()
  "Display buffer process status."
  (and mode-line-process (list " {" mode-line-process "}")))

(defsubst mode-line//position ()
  (propertize " L%l C%c %p %I " 'face 'font-lock-constant-face))

(defvar mode-line--center-margin 1)
(defvar mode-line-default-format '("%e" (:eval (mode-line//generate))))
(defvar mode-line-config-alist
  '(((not (eq mode-line--current-window (selected-window)))
     :segments (mode-line//window-number
                mode-line//buffer-id
                mode-line//buffer-major-mode
                mode-line//process
                mode-line//position)
     :center nil)
    ((or (memq major-mode '(dired-mode))
         (and (not (derived-mode-p 'text-mode 'prog-mode))
              (string-match-p "^\\*" (buffer-name))))
     :segments (mode-line//window-number
                mode-line//buffer-id
                mode-line//buffer-major-mode
                mode-line//buffer-status
                mode-line//process)
     :center nil)
    (t :segments (mode-line//window-number
                  mode-line//buffer-id
                  mode-line//buffer-major-mode
                  mode-line//buffer-status
                  mode-line//flycheck
                  mode-line//process
                  mode-line//position)
       :center t)))

(defsubst mode-line//format-line ($lhs $chs $rhs)
  (let* ((lw (string-width $lhs))
         (cw (string-width $chs))
         (rw (string-width $rhs))
         (tw (window-total-width))
         (margin (/ (- tw (+ lw rw cw)) 2)))
    (setq $chs
          (if (>= margin mode-line--center-margin)
              (concat (make-string margin ?\s)
                      $chs
                      (make-string (max 0 (- tw (+ lw cw rw margin))) ?\s))
            (make-string (max mode-line--center-margin (- tw (+ lw rw))) ?\s)))
    (concat $lhs $chs $rhs)))


(defmacro mode-line//compile (&rest $segments)
  `(defun mode-line//generate ()
     "Generate mode-line."
     (unless (bound-and-true-p eldoc-mode-line-string)
       (cond
        ,@(mapcar
           (lambda (config)
             (list (car config)
                   (let ((segments (plist-get (cdr config) :segments))
                         (show-center-p (plist-get (cdr config) :center)))
                     `(list ,@(mapcar #'list segments)
                            ,(and show-center-p 'mode-line-misc-info)))))
           (or $segments mode-line-config-alist))))))

(mode-line//compile)

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
                '((projectile-mode
                   ("["
                    projectile-mode-line
                    (:propertize "|" face font-lock-comment-face)
                    core-current-desktop-name
                    "]"))
                  ;; (company-mode company-lighter)
                  (global-mode-string ("" global-mode-string " ")))))

(provide 'core-mode-line)

