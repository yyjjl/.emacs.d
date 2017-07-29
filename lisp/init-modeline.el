(defun mode-line|buffer-id ()
  "Display buffer id in mode-line.
Default format 'window-number %[%b (host-address) %]'
If function `window-numbering-mode' enabled window-number will be showed.
If buffer is narrowed, there will be a '><' around window-number.
If buffer file is a remote file, host-address will be showed"
  (let* ((host (and default-directory
                    (let ((tmp (file-remote-p default-directory)))
                      (and tmp (split-string tmp ":")))))
         (num (if (bound-and-true-p window-numbering-mode)
                  (let ((num-str (condition-case nil
                                     (window-numbering-get-number-string)
                                   (error " "))))
                    (if (buffer-narrowed-p)
                        (concat ">" num-str "<") num-str))
                ""))
         (real-id (concat (propertize "%b" 'face font-lock-keyword-face)
                          (if (and host (cdr host))
                              (propertize (concat "(" (cadr host) ")")
                                          'face font-lock-string-face)
                            ""))))
    (concat num " %[" real-id "%]")))

(defun mode-line|buffer-major-mode ()
  "Display buffer major mode in mode-line."
  (propertize " %m "
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
          (if (buffer-temporary-p)
              "tmp|"
            (when (buffer-modified-p)
              (propertize "mod|"
                          'face nil
                          'help-echo "Buffer has been modified")))
          (when buffer-read-only
            (propertize "read-only|"
                        'face nil
                        'help-echo "Buffer is read-only"))
          (let ((buf-coding (format "%s" buffer-file-coding-system)))
            (if (string-match "\\(dos\\|unix\\|mac\\)" buf-coding)
                (match-string 1 buf-coding)
              buf-coding))
          ")"))

(defun mode-line|flycheck ()
  "Display flycheck status in mode-line."
  (if (bound-and-true-p flycheck-mode)
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
                                         'face 'flycheck-fringe-warning)))))))

(defun mode-line|vc ()
  "Display `version-control' status."
  (and (buffer-file-name (current-buffer)) vc-mode))

(defun mode-line|process ()
  "Display buffer process status."
  (let ((proc (get-buffer-process (current-buffer)))
        (msg (format-mode-line mode-line-process)))
    (if (and proc (process-live-p proc))
        (format " {%s@%s} " msg (process-id proc))
      (if (> (length msg) 0)
          (format " {%s} " msg)
        ""))))

(defvar mode-line|center-margin 3)
(defvar mode-line|special-string-function nil)
(defvar mode-line|default-format '("%e" (:eval (mode-line|generate))))
(defun mode-line|special-string ()
  (propertize (cond ((stringp mode-line|special-string-function)
                     mode-line|special-string-function)
                    ((or (functionp mode-line|special-string-function)
                         (fboundp mode-line|special-string-function))
                     (funcall mode-line|special-string-function))
                    (t ""))
              'face 'font-lock-doc-face))
(make-variable-buffer-local 'mode-line|special-string-function)
(defun mode-line|generate ()
  "Generate mode-line."
  (unless (bound-and-true-p eldoc-mode-line-string)
    (let* ((lhs (format-mode-line
                 (concat (mode-line|buffer-id)
                         (mode-line|process)
                         (mode-line|buffer-major-mode)
                         (mode-line|buffer-status))))
           (chs (concat (propertize (or display-time-string "")
                                    'face 'font-lock-negation-char-face)
                        (mode-line|special-string)))
           (rhs (format-mode-line
                 (concat (mode-line|vc) " "
                         (mode-line|flycheck) " "
                         (propertize "%I [%l:%c] %p%% "
                                     'face 'font-lock-constant-face))))
           (lw (string-width lhs))
           (cw (string-width chs))
           (rw (string-width rhs))
           (tw (window-total-width))
           (margin (/ (- tw (+ lw rw cw)) 2)))
      (if (>= margin mode-line|center-margin)
          (format (format " %%s%%%ds%%%ds" (+ cw margin) (+ rw  margin 1))
                  lhs chs rhs)
        (format (format " %%s%%%ds" (+ 1 (- tw lw)))
                lhs rhs)))))

;; Setup `mode-line-format'
(window-numbering-mode 1)
(setq-default mode-line-format mode-line|default-format)

(require 'uniquify)

(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator " • ")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

(when (fboundp 'define-fringe-bitmap)
  (define-fringe-bitmap 'right-curly-arrow
    [#b11111111
     #b11111111
     #b00000011
     #b00000011
     #b00000011
     #b00000011
     #b00000011
     #b00000011])
  (define-fringe-bitmap 'left-curly-arrow
    [#b11000000
     #b11000000
     #b11000000
     #b11000000
     #b11000000
     #b11000000
     #b11111111
     #b11111111]))

(provide 'init-modeline)
