;;; -*- lexical-binding: t; -*-

(defvar-local mode-line-show-root-p t)

(defvar-local mode-line--cached-git-branch nil)
(defvar-local mode-line--cached-relative-directory nil)
(defvar-local mode-line--cached-directory nil)
(defvar-local mode-line--cached-root nil)
(defvar mode-line--current-window (frame-selected-window))

(defvar mode-line-multi-edit-alist
  '((iedit-mode . (iedit-counter))
    (multiple-cursors-mode . (mc/num-cursors))
    (elpy-multiedit-overlays . (length elpy-multiedit-overlays))))

(defvar mode-line--center-margin 1)
(defvar mode-line-default-format '(:eval (mode-line//generate)))
(defvar mode-line--default-segments '(mode-line//window-number
                                      mode-line//buffer-id
                                      mode-line//buffer-major-mode))
(defvar mode-line-config-alist
  `(((or (memq major-mode '(dired-mode))
         (and (not (derived-mode-p 'text-mode 'prog-mode))
              (eq (aref (buffer-name) 0) ?*)))
     :segments (,@mode-line--default-segments
                mode-line//process)
     :root current)
    ((not (eq mode-line--current-window (selected-window)))
     :segments (,@mode-line--default-segments
                mode-line//process
                mode-line//position)
     :root project)
    (t :segments (,@mode-line--default-segments
                  mode-line//process
                  mode-line//buffer-status
                  mode-line//flycheck
                  mode-line//flymake
                  mode-line//position
                  mode-line//git-info)
       :misc t
       :root project)))

(autoload 'image-get-display-property "image-mode" nil)

(defsubst mode-line//window-active-p ()
  (eq mode-line--current-window (selected-window)))

(defsubst mode-line//window-number ()
  (and (bound-and-true-p winum-mode)
       (let ((narrow-p (buffer-narrowed-p)))
         (propertize (concat
                      (if narrow-p ">" "-")
                      (ignore-errors (winum-get-number-string))
                      (if narrow-p "<" "-"))
                     'face
                     (if (mode-line//window-active-p)
                         'winum-face
                       'winum-inactive-face)))))

(defsubst mode-line//git-info ()
  (when buffer-file-name
    (and (not (eq mode-line--cached-git-branch 'none))
         (or mode-line--cached-git-branch
             (when (require 'magit nil :noerror)
               (setq mode-line--cached-git-branch
                     (if-let (branch (magit-get-current-branch))
                         (propertize branch 'face 'font-lock-doc-face)
                       'none)))))))

(defsubst mode-line//relative-directory ()
  (or (and mode-line--cached-directory
           (equal mode-line--cached-directory default-directory)
           mode-line--cached-relative-directory)
      (let ((root (file-truename (or (projectile-project-root) default-directory)))
            (directory (file-truename default-directory)))
        (setq mode-line--cached-directory default-directory)
        (setq mode-line--cached-root (abbreviate-file-name root))
        (setq mode-line--cached-relative-directory
              (if (and (string-prefix-p root directory) (buffer-file-name))
                  (substring directory (length root))
                ;; (mapconcat
                ;;  (lambda (x) (if (equal x "") "" (substring x 0 1)))
                ;;  (split-string (substring directory (length root)) "/")
                ;;  "/")
                "")))))

(defsubst mode-line//buffer-id ()
  "Display buffer id in mode-line."
  (list "%["
        (propertize (mode-line//relative-directory) 'face 'font-lock-string-face)
        '(:propertize mode-line-buffer-identification face font-lock-keyword-face)
        "%]"))

(defsubst mode-line//buffer-major-mode ()
  "Display buffer major mode in mode-line."
  '(:propertize mode-name face font-lock-builtin-face))

(defsubst mode-line//buffer-status ()
  "Display buffer status.

Whether it is temporary file, whether it is modified, whether is
read-only, and `buffer-file-coding-system'"
  (list "("
        (cl-loop for (symbol . expr) in mode-line-multi-edit-alist
                 when (and (boundp symbol) (symbol-value symbol))
                 do (when-let (count (eval expr))
                      (cl-return (propertize (format "e:%d " count)
                                             'face font-lock-constant-face))))
        (when (and (boundp 'text-scale-mode-amount)
                   (/= text-scale-mode-amount 0))
          (propertize (format "%+d " text-scale-mode-amount)
                      'face font-lock-doc-face))
        (when (or defining-kbd-macro executing-kbd-macro)
          (propertize "macro " 'face font-lock-variable-name-face))
        (when (buffer-temporary-p)
          (propertize "tmp " 'face font-lock-comment-face))
        (when (buffer-modified-p)
          (propertize "m " 'face font-lock-negation-char-face))
        (when buffer-read-only
          (propertize (if view-mode "view " "ro ") 'face font-lock-string-face))
        (when visual-line-mode
          (propertize "vs " 'face font-lock-type-face))
        (when visible-mode "Vis ")
        (when (buffer-base-buffer) "I ")
        (when (eq major-mode 'image-mode)
          (cl-destructuring-bind (width . height)
              (image-size (image-get-display-property) :pixels)
            (if image-type
                (format "%dx%d(%s) " width height image-type)
              (format "%dx%d " width height))))
        (let ((buffer-encoding (format "%s" buffer-file-coding-system)))
          (if (string-match "\\(dos\\|unix\\|mac\\)" buffer-encoding)
              (match-string 1 buffer-encoding)
            buffer-encoding))
        ")"))

(defsubst mode-line//flycheck ()
  "Display flycheck status in mode-line."
  (when (bound-and-true-p flycheck-mode)
    (pcase flycheck-last-status-change
      (`not-checked '(:propertize "Waiting" face font-lock-comment-face))
      (`no-checker '(:propertize "No" face font-lock-comment-face))
      (`running '(:propertize "Running" face font-lock-doc-face))
      (`errored '(:propertize "Error" face flycheck-fringe-error))
      (`interrupted '(:propertize "Interrupted" face flycheck-fringe-warning))
      (`suspicious '(:propertize "???" face flycheck-fringe-error))
      (`finished
       (let-alist (flycheck-count-errors flycheck-current-errors)
         `("["
           (lsp-mode "LSP:")
           (:propertize ,(format "%d" (or \.error 0)) face compilation-error)
           (:propertize ,(format ":%d" (or \.warning 0)) face compilation-warning)
           (:propertize ,(format ":%d" (or \.info 0)) face compilation-info)
           "]"))))))

(defsubst mode-line//flymake ()
  "Produce a pretty minor mode indicator."
  (when (bound-and-true-p flymake-mode)
    (let* ((error-count 0)
           (warning-count 0)
           (note-count 0)
           (error-severity (warning-numeric-level :error))
           (warning-severity (warning-numeric-level :warning))
           (note-severity (warning-numeric-level :debug)))
      (maphash
       (lambda (_b state)
         (dolist (diag (flymake--backend-state-diags state))
           ;; In this version, level of :error is 3
           (let ((severity
                  (flymake--lookup-type-property (flymake--diag-type diag)
                                                 'severity
                                                 error-severity)))
             (cond ((>= severity error-severity)
                    (cl-incf error-count))
                   ((>= severity warning-severity)
                    (cl-incf warning-count))
                   ((>= severity note-severity)
                    (cl-incf note-count))))))
       flymake--backend-state)
      `("["
        (lsp-mode "LSP:")
        (:propertize ,(format "%d" error-count) face compilation-error)
        (:propertize ,(format ":%d" warning-count) face compilation-warning)
        (:propertize ,(format ":%d" note-count) face compilation-info)
        "]"))))

(defsubst mode-line//process ()
  "Display buffer process status."
  `(:propertize ,mode-line-process face font-lock-string-face))

(defsubst mode-line//position ()
  (propertize "%l:%c %p %I" 'face 'font-lock-constant-face))

(defsubst mode-line//format-line (-lhs -chs -rhs)
  (let* ((lw (string-width -lhs))
         (cw (string-width -chs))
         (rw (string-width -rhs))
         (tw (window-total-width))
         (margin (/ (- tw (+ lw rw cw)) 2)))
    (setq -chs
          (if (>= margin mode-line--center-margin)
              (concat (make-string margin ?\s)
                      -chs
                      (make-string (max 0 (- tw (+ lw cw rw margin))) ?\s))
            (make-string (max mode-line--center-margin (- tw (+ lw rw))) ?\s)))
    (concat -lhs -chs -rhs)))


(defmacro mode-line//compile (&rest -segments)
  `(defun mode-line//generate ()
     "Generate mode-line."
     (unless (bound-and-true-p eldoc-mode-line-string)
       (replace-regexp-in-string
        (rx (>= 2 space)) " "
        (format-mode-line
         (cond
          ,@(mapcar
             (lambda (config)
               (list
                (car config)
                (let ((segments (plist-get (cdr config) :segments))
                      (show-misc-p (plist-get (cdr config) :misc))
                      (show-root-p (plist-get (cdr config) :root)))
                  (delete
                   nil
                   `(list ,@(cdr (mapcan (lambda (segment) (list " " `(,segment))) segments))
                          ,(when show-misc-p ''mode-line-misc-info)
                          ,@(when show-root-p
                              (list " "
                                    `'(mode-line-show-root-p
                                       ,(cl-case show-root-p
                                          (project 'mode-line--cached-root)
                                          (current '(:eval (abbreviate-file-name default-directory))))))))))))
             (or -segments mode-line-config-alist))))))))

(defun mode-line*trace-magit-checkout (-fn &rest -args)
  (let ((buffer (current-buffer)))
    (apply -fn -args)
    (with-current-buffer buffer
      (setq mode-line--cached-git-branch nil)
      (mode-line//git-info))))

(with-eval-after-load 'magit
  (advice-add 'magit-checkout :around #'mode-line*trace-magit-checkout))

(defun mode-line*set-selected-window (&rest _)
  "Sets `mode-line--current-window' appropriately"
  (let ((win (frame-selected-window)))
    (unless (minibuffer-window-active-p win)
      (setq mode-line--current-window win))))
(advice-add #'select-window :after #'mode-line*set-selected-window)

;; Setup `mode-line-format'
(define-hook! mode-line|after-init-hook (after-init-hook)
  (winum-mode 1)
  (setq-default mode-line-format mode-line-default-format)
  (setq-default mode-line-buffer-identification '("%b"))
  (setq-default mode-line-misc-info
                '((company-search-mode (" " company-search-lighter))
                  " " global-mode-string)))

(mode-line//compile)

(provide 'core-modeline)