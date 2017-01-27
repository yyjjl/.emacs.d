(defface  window-numbering-face
  '((t (:foreground "DeepPink" :weight bold)))
  "Window number face"
  :group 'mode-line)

(defface flycheck-mode-line-off-face
  '((t :weight normal :foreground "tan"))
  "Face for the modeline when flycheck mode is not activate"
  :group 'flycheck-faces)

(defface flycheck-mode-line-run-face
  '((t :weight normal :foreground "DeepSkyBlue"))
  "Face for the modeline in buffers with Flycheck running."
  :group 'flycheck-faces)

(defface flycheck-mode-line-error-face
  '((t :weight normal :foreground "DarkRed"))
  "Face for the modeline when flycheck exited with an error"
  :group 'flycheck-faces)

(defface flycheck-mode-line-ok-face
  '((t  :weight normal :foreground "DarkOliveGreen"))
  "Face for the modeline when buffer is ok"
  :group 'flycheck-faces)

(defface flycheck-mode-line-failed-face
  '((t :weight normal :foreground "OrangeRed"))
  "Face for the modeline when flycheck failed to parse buffer"
  :group 'flycheck-faces)

(defface flycheck-mode-line-result-face
  '((t :weight normal :foreground "Orange"))
  "Face for the modeline when the buffer has warnings or errors"
  :group 'flycheck-faces)


(add-hook 'after-init-hook
          '(lambda ()
             (window-numbering-mode 1)
             (setq-default mode-line-format
                           '("%e" (:eval (generate-mode-line))))))

(defun mode-line-buffer-id ()
    (let* ((host (and  default-directory
                      (file-remote-p default-directory 'host)))
           (num (if (bound-and-true-p window-numbering-mode)
                    (window-numbering-get-number-string)
                  ""))
           (real-id (propertize (if host
                                    (concat "%b" "@" host)
                                  "%b")
                                'face font-lock-keyword-face)))
      (concat num " %[" real-id "%]")))

(defun mode-line-buffer-major-mode ()
  (concat  (propertize " %m " 'face 'font-lock-builtin-face
                       'mouse-face 'mode-line-highlight
                       'local-map (let ((map (make-sparse-keymap)))
                                    (define-key map [mode-line mouse-2]
                                      'describe-mode)
                                    (define-key map [mode-line down-mouse-1]
                                      `(menu-item
                                        ,(purecopy "Menu Bar")
                                        ignore
                                        :filter (lambda (_)
                                                  (mouse-menu-major-mode-map))))
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
            (propertize "ro|"
                        'face nil
                        'help-echo "Buffer is read-only"))
          (let ((buf-coding (format "%s" buffer-file-coding-system)))
            (if (string-match "\\(dos\\|unix\\|mac\\)" buf-coding)
                (match-string 1 buf-coding)
              buf-coding))
          ")"))

(defun mode-line-workgroup2 ()
  (when (and (boundp 'workgroups-mode)
            (symbol-value 'workgroups-mode))
    (let ((name (condition-case nil
                    (wg-workgroup-name (wg-current-workgroup))
                  (error ""))))
      (if (string= name "")
          (setq name "none"))
      (concat " <"
              (propertize name
                          'face 'font-lock-function-name-face
                          'help-echo (format "Current workgroup is %s" name))
              ">"))))

(defun mode-line-flycheck ()
  (if (and (boundp 'flycheck-mode)
          (symbol-value 'flycheck-mode))
      (let ((str (s-trim (flycheck-mode-line-status-text)))
            (msg "")
            (face nil))
        (cond ((string= str "")
               (setq msg "ok")
               (setq face 'flycheck-mode-line-ok-face))
              ((string= str "*")
               (setq msg "run")
               (setq face 'flycheck-mode-line-run-face))
              ((string= str "-")
               (setq msg "no")
               (setq face 'flycheck-mode-line-off-face))
              ((string= str "!")
               (setq msg "fail")
               (setq face 'flycheck-mode-line-failed-face))
              ((string= str "?")
               (setq msg "err")
               (setq face 'flycheck-mode-line-error-face))
              (t (setq face 'flycheck-mode-line-result-face)
                 (setq msg (s-replace ":" "" str))))
        (concat " " (propertize msg 'face face)))))

(defun mode-line-vc ()
  (when (and (buffer-file-name (current-buffer)) vc-mode)
    (propertize (format-mode-line '(vc-mode vc-mode))
                'face 'font-lock-constant-face)))

(defun mode-line-process ()
  (when mode-line-process
    (concat " {"
            (format-mode-line   mode-line-process)
            "} ")))

(defun generate-mode-line ()
  (let ((lhs (format-mode-line
              (concat (mode-line-buffer-id)
                      (mode-line-buffer-major-mode)
                      (mode-line-buffer-status))))
        (rhs (format-mode-line
              (concat (mode-line-vc)
                      ;; (mode-line-workgroup2)
                      (mode-line-process)
                      (mode-line-flycheck)
                      " %I(%l-%c)%p%%"))))
    (format (format " %%s%%%ds" (or (- (window-total-width)
                                       (length (format-mode-line lhs))
                                       1)
                                     0))
            lhs rhs)))

(require 'uniquify)

(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator " • ")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

(provide 'init-modeline)

;;; init-modeline ends here
