(defun mode-line-buffer-id ()
  (let* ((host (and default-directory
                   (let ((tmp (file-remote-p default-directory)))
                     (and tmp (split-string tmp ":")))))
         (num (if (bound-and-true-p window-numbering-mode)
                  (window-numbering-get-number-string)
                ""))
         (real-id (concat (propertize "%b" 'face font-lock-keyword-face)
                          (if (and host (cdr host))
                              (propertize (concat "(" (cadr host) ")")
                                          'face font-lock-string-face)
                            ""))))
    (concat num " %[" real-id "%]")))

(defun mode-line-buffer-major-mode ()
  (let ((mm (format-mode-line "%m")))
    (propertize (concat " "
                         (if (string= mm "") (symbol-name major-mode) mm)
                         " ")
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
                  map))))

(defun mode-line-buffer-status ()
  (concat "("
          (if (is-buffer-file-temp)
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

(defun mode-line-flycheck ()
  (if (bound-and-true-p flycheck-mode)
      (let* ((str (s-trim (flycheck-mode-line-status-text)))
             (infos '(("" "ok" flycheck-fringe-info)
                      ("*" "run" flycheck-running)
                      ("-" "no" flycheck-fringe-error)
                      ("!" "fail" flycheck-fringe-error)
                      ("?" "error" flycheck-fringe-error)))
             (info (assoc-string str infos))
             (msg (if info (cadr info) (substring str 1)))
             (face (if info (caddr info) 'flycheck-fringe-warning)))
        (propertize msg 'face face))))

(defun mode-line-vc ()
  (and (buffer-file-name (current-buffer)) vc-mode))

(defun mode-line-process ()
  (let ((proc (get-buffer-process (current-buffer))))
    (if (not proc) ""
      (if (process-live-p proc)
          (format " {:run %s} " (process-id proc))
        (format " {:exit %s} " (process-exit-status proc))))))

(defun generate-mode-line ()
  (let ((lhs (format-mode-line
              (concat (mode-line-buffer-id)
                      (mode-line-process)
                      (mode-line-buffer-major-mode)
                      (mode-line-buffer-status))))
        (rhs (format-mode-line
              (concat (mode-line-vc) " "
                      (mode-line-flycheck) " "
                      (propertize "%I [%l:%c] %p%% "
                                  'face 'font-lock-constant-face)))))
    (format (format " %%s%%%ds" (or (- (window-total-width)
                                       (string-width (format-mode-line lhs)))
                                     0))
            lhs rhs)))

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

;;; init-modeline ends here
