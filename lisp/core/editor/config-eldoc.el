;;; -*- lexical-binding: t; -*-

(defmacro when! (-cond &rest -body)
  "Similary to `when', but -COND is eval during byte-compiling."
  (declare (indent 1) (debug t))
  `(if! ,-cond (progn ,@-body)))

(defmacro if! (-cond -if-statement &rest -else-body)
  "Similary to `if', but -COND is eval during byte-compiling."
  (declare (indent 2) (debug t))
  (if (eval -cond)
      -if-statement
    `(progn ,@-else-body)))

(when! ymacs-editor-use-childframe-p
  (defvar ymacs-editor-doc-buffer "*lsp-signature*")

  (defun ymacs-editor//posframe-poshandler (-info)
    (if (active-minibuffer-window)
        (posframe-poshandler-point-bottom-left-corner-upward -info)
      (let* ((x-pixel-offset (plist-get -info :x-pixel-offset))
             (window (plist-get -info :parent-window))
             (position-info (plist-get -info :position-info))
             (x (+ (car (window-inside-pixel-edges window))
                   (- (or (car (posn-x-y position-info)) 0)
                      (or (car (posn-object-x-y position-info)) 0))
                   x-pixel-offset)))
        (if (> x (/ (frame-pixel-width) 2))
            (posframe-poshandler-frame-bottom-center -info)
          (posframe-poshandler-frame-bottom-left-corner -info)))))

  (defun ymacs-editor//eldoc-message (-fmt &rest -args)
    (if (or (not ymacs-editor-use-childframe-p)
            (company--active-p)         ; company-mode is active
            (bound-and-true-p lsp-signature-mode))
        (progn
          (posframe-hide ymacs-editor-doc-buffer)
          (apply #'eldoc-minibuffer-message -fmt -args))
      (if -fmt
          (posframe-show
           ymacs-editor-doc-buffer
           :string (apply #'format-message -fmt -args)
           :hidehandler #'posframe-hidehandler-when-buffer-switch
           :poshandler #'ymacs-editor//posframe-poshandler
           :background-color (face-attribute 'tooltip :background)
           :height 6
           :width (min 150 (max 80 (/ (* (frame-width) 3) 4)))
           :border-width 10)
        (posframe-hide ymacs-editor-doc-buffer)))))

(after! eldoc
  (when! ymacs-editor-use-childframe-p
    (setq-default eldoc-message-function #'ymacs-editor//eldoc-message))
  (setq-default eldoc-documentation-function #'eldoc-documentation-compose-eagerly))
