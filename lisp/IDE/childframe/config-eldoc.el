;;; -*- lexical-binding: t; -*-

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
  (if (bound-and-true-p lsp-signature-mode)
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

      (eval-if-has-feature! lsp
          (unless (and (bound-and-true-p lsp-mode)
                       (and lsp--hover-saved-bounds (lsp--point-in-bounds-p lsp--hover-saved-bounds)))
            (posframe-hide ymacs-editor-doc-buffer))
        (posframe-hide ymacs-editor-doc-buffer)))))

(when ymacs-eldoc-use-childfeame-p
  (after! eldoc
    (setq-default eldoc-message-function #'ymacs-editor//eldoc-message)))
