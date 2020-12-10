;;; -*- lexical-binding: t; -*-

(declare-function gud-find-c-expr "gud")

(defsubst ymacs-debug//gdb-running-p ()
  (eq gud-minor-mode 'gdbmi))

(defun ymacs-debug//find-expr (&rest _args)
  (let ((data (save-match-data (gud-find-c-expr))))
    (when (string-match-p "\n" data)
      (setq data (thing-at-point 'symbol)))
    (read-string "Print: " data)))

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

(define-minor-mode ymacs-debug-command-buffer-mode
  "A mode for adding keybindings to comamnd buffer"
  nil nil
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-o") ymacs-debug-running-session-mode-map)
    map))

(define-minor-mode ymacs-debug-info-buffer-mode
  "A mode for adding keybindings to comamnd buffer"
  nil nil
  ymacs-debug-info-mode-map)

(define-minor-mode ymacs-debug-running-session-mode
  "A mode for adding keybindings to running sessions"
  nil nil
  ymacs-debug-running-session-mode-map
  (if ymacs-debug-running-session-mode
      (progn
        (setq ymacs-debug--buffer-read-only buffer-read-only)
        (unless ymacs-debug--dap-cookie
          (setq ymacs-debug--dap-cookie
                (face-remap-add-relative 'header-line 'doom-modeline-debug-visual)))
        (setq buffer-read-only t))

    (when buffer-read-only
      (setq buffer-read-only ymacs-debug--buffer-read-only))
    (setq ymacs-debug--buffer-read-only nil)

    (when ymacs-debug--dap-cookie
      (face-remap-remove-relative ymacs-debug--dap-cookie))
    (setq ymacs-debug--dap-cookie nil))

  (force-mode-line-update))
