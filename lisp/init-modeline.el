(defun mode-line|window-number ()
  (let* ((num-str (concat
                   " "
                   (and (bound-and-true-p window-numbering-mode)
                        (ignore-errors (window-numbering-get-number-string)))
                   " ")))
    (add-face-text-property 0 (length num-str)
                            '(:inherit mode-line-numbering-face)
                            nil num-str)
    num-str))

(defun mode-line|buffer-id ()
  "Display buffer id in mode-line.
Default format 'window-number %[%b (host-address) %]'
If function `window-numbering-mode' enabled window-number will be showed.
If buffer is narrowed, there will be a '><' around window-number.
If buffer file is a remote file, host-address will be showed"
  (let* ((host (and default-directory
                    (let ((tmp (file-remote-p default-directory)))
                      (and tmp (split-string tmp ":")))))
         (real-id (concat
                   (propertize (format-mode-line mode-line-buffer-identification)
                               'face 'font-lock-keyword-face)
                   (if (and host (cdr host))
                       (propertize (concat "(" (cadr host) ")")
                                   'face font-lock-string-face)
                     ""))))
    (concat " %[" real-id "%] ")))

(defun mode-line|buffer-major-mode ()
  "Display buffer major mode in mode-line."
  (propertize "%m "
              'face 'font-lock-builtin-face
              'help-echo (symbol-name major-mode)
              'mouse-face 'mode-line-highlight
              'local-map
              (let ((map (make-sparse-keymap)))
                (define-key map [mode-line mouse-2]
                  'describe-mode)
                (define-key map [mode-line down-mouse-1]
                  `(menu-item
                    ,(purecopy "Menu Bar")
                    ignore
                    :filter (lambda (_) (mouse-menu-major-mode-map))))
                map)))

(defun mode-line|buffer-status ()
  "Display buffer status.
Whether it is temporary file, whether it is modified, whether is read-only,
and `buffer-file-coding-system'"
  (concat "("
          (when (buffer-temporary-p)
            (propertize "T|" 'help-echo "Buffer is temporary"))
          (when (buffer-modified-p)
            (propertize "M|" 'help-echo "Buffer has been modified"))
          (when buffer-read-only
            (propertize "R|" 'help-echo "Buffer is read-only"))
          (when (buffer-narrowed-p)
            (propertize "N|" 'help-echo "Buffer is read-only"))
          (propertize (symbol-name buffer-file-coding-system)
                      'help-echo "Buffer encoding")
          ")"))

(defun mode-line|flycheck ()
  "Display flycheck status in mode-line."
  (if (bound-and-true-p flycheck-mode)
      (concat
       (pcase flycheck-last-status-change
         (`not-checked (propertize "waiting" 'face 'font-lock-comment-face))
         (`no-checker  (propertize "no-checker" 'face 'font-lock-comment-face))
         (`running (propertize "running" 'face  'font-lock-doc-face))
         (`errored (propertize "error" 'face  'flycheck-fringe-error))
         (`interrupted (propertize "interrupted" 'face  'flycheck-fringe-warning))
         (`suspicious (propertize "???" 'face 'flycheck-fringe-error))
         (`finished (let-alist (flycheck-count-errors flycheck-current-errors)
                      (concat (propertize (format "E%s" (or .error 0))
                                          'face 'flycheck-fringe-error)
                              "|"
                              (propertize (format "W%s" (or .warning 0))
                                          'face 'flycheck-fringe-warning)))))
       " ")))

(defun mode-line|vc ()
  "Display `version-control' status."
  (and (buffer-file-name (current-buffer))
       (concat vc-mode " ")))

(defun mode-line|process ()
  "Display buffer process status."
  (let ((proc (get-buffer-process (current-buffer)))
        (msg (format-mode-line mode-line-process)))
    (if (and proc (process-live-p proc))
        (format "{%s@%s} " msg (process-id proc))
      (if (> (length msg) 0)
          (format "{%s} " msg)
        ""))))

(defvar mode-line|center-margin 1)
(defvar mode-line|default-format '("%e" (:eval (mode-line|generate))))
(defun mode-line|generate ()
  "Generate mode-line."
  (unless (bound-and-true-p eldoc-mode-line-string)
    (let* ((lhs (format-mode-line
                 (concat (mode-line|window-number)
                         (mode-line|buffer-id)
                         (mode-line|buffer-major-mode)
                         (mode-line|buffer-status))))
           (chs (format-mode-line global-mode-string))
           (rhs (format-mode-line
                 (concat (mode-line|flycheck)
                         (mode-line|process)
                         (mode-line|vc)
                         (propertize "%I [L%l:C%c] %p%%"
                                     'face 'font-lock-constant-face))))
           (lw (string-width lhs))
           (cw (string-width chs))
           (rw (string-width rhs))
           (tw (window-total-width))
           (margin (/ (- tw (+ lw rw cw)) 2)))
      (if (>= margin mode-line|center-margin)
          (format (format "%%s%%%ds%%%ds" (+ cw margin) (- tw (+ lw cw margin)))
                  lhs chs rhs)
        (format (format "%%s%%%ds" (- tw lw))
                lhs rhs)))))

;; Setup `mode-line-format'
(defhook mode-line|after-init (after-init-hook)
  (window-numbering-mode 1)
  (setq-default mode-line-format mode-line|default-format)
  (setq-default frame-title-format
                '(:eval (let ((fn (buffer-file-name)))
                          (if fn
                              (abbreviate-file-name fn)
                            (buffer-name))))))

(provide 'init-modeline)
