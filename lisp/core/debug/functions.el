;;; -*- lexical-binding: t; -*-

(defun ymacs-debug//gud-source-buffer-p (-buffer _alist)
  (let* ((buffer (get-buffer -buffer))
         (regex (rx string-start "*gud" (*? not-newline) "*" string-end)))
    (with-current-buffer buffer
      (cond
       ((string-match-p regex (buffer-name))
        (setq ymacs-popup--matched-rule 'gud-command-window))

       ((eq major-mode 'gdb-inferior-io-mode)
        (setq ymacs-popup--matched-rule 'gdb-io-window))

       ((provided-mode-derived-p major-mode 'gdb-parent-mode)
        (setq ymacs-popup--matched-rule 'gdb-info-window))

       ((and gud-minor-mode (buffer-file-name))
        (setq ymacs-popup--matched-rule 'gud-source-window))))))

(defun ymacs-debug//display-buffer (-buffer -alist)
  (with-current-buffer -buffer
    (cl-case ymacs-popup--matched-rule
      (gud-command-window

       (setq ymacs-popup--matched-rule '(:side right :size 0.5))
       (when-let (window (ymacs-popup//display-buffer-action -buffer -alist))
         (set-window-parameter window 'no-delete-other-windows t)
         (set-window-dedicated-p window t)
         window))

      ((gdb-info-window gdb-io-window)

       (let ((io-window-p (eq ymacs-popup--matched-rule 'gdb-io-window)))
         (setf (alist-get 'side -alist) 'bottom)
         (setf (alist-get 'slot -alist) (if io-window-p 2 1))
         (setf (alist-get 'window-height -alist) 0.35)
         (when-let (window (or (display-buffer-reuse-window -buffer -alist)
                               (display-buffer-in-side-window -buffer -alist)))
           (set-window-dedicated-p window t)
           (unless io-window-p
             (select-window window))
           window)))

      (gud-source-window

       (setf (alist-get 'inhibit-same-window -alist) nil)
       (or (display-buffer-use-some-window -buffer -alist)
           (display-buffer-pop-up-window -buffer -alist))))))

(defun ymacs-debug//resuse-session ()
  (when (and (buffer-live-p (bound-and-true-p gud-comint-buffer))
             (process-live-p (get-buffer-process gud-comint-buffer))
             (buffer-file-name))
    (gud-find-file (buffer-file-name))
    (message "Reuse gud session")))

(defsubst ymacs-debug//gdb-running-p ()
  (eq gud-minor-mode 'gdbmi))

(defun ymacs-debug//find-expr (&rest _args)
  (save-match-data (read-string "Print: " (thing-at-point 'secp))))

(defun ymacs-debug//show-help (-message)
  (if (null -message)
      (when (window-live-p ymacs-debug--help-window)
        (delete-window ymacs-debug--help-window))
    (let ((buffer (get-buffer-create ymacs-debug--help-buffer-name)))
      (with-current-buffer buffer
        (setq window-size-fixed t)
        (setq mode-line-format nil)
        (setq header-line-format nil)
        (setq tab-line-format nil)
        (setq cursor-type nil)
        (setq display-line-numbers nil)
        (setq display-fill-column-indicator nil)
        (erase-buffer)
        (insert -message)

        (let ((window
               (or (when (window-live-p ymacs-debug--help-window)
                     (if (eq (window-frame ymacs-debug--help-window) (selected-frame))
                         (set-window-buffer ymacs-debug--help-window buffer)
                       (delete-window ymacs-debug--help-window))
                     ymacs-debug--help-window)
                   (setq ymacs-debug--help-window
                         (display-buffer-in-side-window buffer '((side . top))))))
              (window-resize-pixelwise t)
              (window-size-fixed nil))
          (set-window-hscroll window 0)
          (set-window-parameter window 'no-delete-other-windows t)
          (fit-window-to-buffer window nil 1)
          (set-window-dedicated-p window t)
          (set-window-parameter window 'no-other-window t))))))


(defun ymacs-debug//before-debug ()
  (ymacs-debug//show-help nil)
  (lv-delete-window)
  ;; FIXME: In Emacs 28.1, restoring window configuration is a built-in feature
  (window-configuration-to-register :debug-windows)

  (delete-other-windows))

(defun ymacs-debug//after-debug (&rest -modes)
  (dolist (buffer ymacs-debug--buffers)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (dolist (mode -modes)
          (funcall mode -1))

        (dolist (window (get-buffer-window-list))
          (when (window-parameter window 'no-delete-other-windows)
            (set-window-parameter window 'no-delete-other-windows nil))))))

  (setq ymacs-debug--buffers nil)
  ;; restore windows
  (with-demoted-errors "%s"
    (jump-to-register :debug-windows)))

(define-minor-mode ymacs-debug-command-buffer-mode
  "A mode for adding keybindings to comamnd buffer"
  nil nil
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-o") ymacs-debug-info-mode-map)
    map))

(define-minor-mode ymacs-debug-info-buffer-mode
  "A mode for adding keybindings to comamnd buffer"
  nil nil
  ymacs-debug-info-mode-map)

(defun ymacs-debug//enable ()
  (cl-pushnew (current-buffer) ymacs-debug--buffers)

  (setq ymacs-debug--buffer-read-only buffer-read-only)
  (unless ymacs-debug--cookie
    (setq ymacs-debug--cookie
          (face-remap-add-relative 'header-line 'ymacs-modeline-debug-visual)))
  (setq buffer-read-only t)

  (tooltip-mode 1)
  (display-line-numbers-mode 1)
  (line-number-mode 1)
  (column-number-mode 1))

(defun ymacs-debug//disable ()
  (tooltip-mode -1)
  (display-line-numbers-mode -1)
  (line-number-mode -1)
  (column-number-mode -1)
  (when buffer-read-only
    (setq buffer-read-only ymacs-debug--buffer-read-only))
  (setq ymacs-debug--buffer-read-only nil)

  (when ymacs-debug--cookie
    (face-remap-remove-relative ymacs-debug--cookie))
  (setq ymacs-debug--cookie nil))

(define-minor-mode ymacs-debug-running-session-mode
  "A mode for adding keybindings to running sessions"
  nil nil
  ymacs-debug-running-session-mode-map

  (if ymacs-debug-running-session-mode
      (ymacs-debug//enable)
    (ymacs-debug//disable))

  (force-mode-line-update))
