;;; -*- lexical-binding: t; -*-

(defvar ymacs-editor-display-help-extra-lines nil)
(defvar ymacs-editor-display-help-max-columns 5)
(defvar ymacs-editor-display-help-column-sep "  ")
(defvar ymacs-editor-display-help-ignore-commands nil)
(defvar ymacs-editor-display-help-window nil)
(defvar ymacs-editor-display-help-buffer-name " *EDITOR-HELP*")

(after! ace-window
  (add-to-list 'aw-ignored-buffers ymacs-editor-display-help-buffer-name))

(defsubst ymacs-editor//display-keys--collect (-keymap)
  (let (keys)
    (when -keymap
      (cl--map-keymap-recursively
       (lambda (key definition)
         (unless (or (memq (aref key 0) '(remap menu-bar)) ; skip remap
                     (memq definition ymacs-editor-display-help-ignore-commands))
           (push (cons (key-description key) definition) keys)))
       -keymap))
    keys))

(defsubst ymacs-editor//display-keys--format (keys)
  (cl-loop
   for (definition . items) in (-group-by #'cdr keys)
   collect
   (format "[%s] %s"
           (mapconcat
            (lambda (item) (propertize (car item) 'face 'help-key-binding))
            items
            "/")
           (cond ((symbolp definition) definition)
                 ((functionp definition) "<anonymous>")
                 (t "<error>")))))


(defsubst ymacs-editor//display-keys--try-columns (-sorted-keys
                                                   -n-columns -n-long-columns -n-rows
                                                   -max-width -sep-width)
  (or (= -n-columns 1)
      (let ((width (* -sep-width (1- -n-columns)))
            (keys -sorted-keys))
        (cl-loop
         for i from 0 to (1- -n-columns)
         for n = (+ -n-rows (if (< i -n-long-columns) 1 0))
         do (progn
              (setq keys (nthcdr (1- n) keys))
              ;; the lengths of -sorted-keys is ordered
              (cl-incf width (length (car keys)))
              (setq keys (cdr keys))))
        (< width -max-width))))

(defsubst ymacs-editor//display-keys--format-table (-sorted-keys
                                                    -n-columns -n-long-columns -n-rows
                                                    -column-sep)
  (let ((rows (make-vector (1+ -n-rows) nil))
        (keys -sorted-keys))
    (cl-loop
     for i from 0 to (1- -n-columns)
     for n = (+ -n-rows (if (< i -n-long-columns) 1 0))
     for pad-width = (length (car (nthcdr (1- n) keys)))
     do (progn
          (dotimes (j n)
            (aset rows j
                  (cons (concat (car keys) (make-string (- pad-width (length (car keys))) ?\ ))
                        (aref rows j)))
            (setq keys (cdr keys)))))
    (string-trim-right
     (mapconcat
      (lambda (row) (string-join (nreverse row) -column-sep))
      rows
      "\n"))))

(defsubst ymacs-editor//display-help--keys (-keys)
  (when-let (keys (ymacs-editor//display-keys--format -keys))
    ;; sort by length
    (setq keys (sort keys (lambda (x y) (< (length x) (length y)))))

    (let* ((n-keys (length keys))
           (max-width (frame-width))
           (max-columns ymacs-editor-display-help-max-columns)
           (sep-width (length ymacs-editor-display-help-column-sep)))
      (cl-loop
       for n-columns from max-columns downto 1
       for n-long-columns = (mod n-keys n-columns)
       for n-rows = (/ n-keys n-columns)
       when (ymacs-editor//display-keys--try-columns
             keys n-columns n-long-columns n-rows max-width sep-width)
       return (ymacs-editor//display-keys--format-table
               keys n-columns n-long-columns n-rows ymacs-editor-display-help-column-sep)))))

(defun ymacs-editor//display-help--get-window ()
  (if (and (window-live-p ymacs-editor-display-help-window)
           (eq (window-frame ymacs-editor-display-help-window) (selected-frame)))
      ;; ymacs-editor-display-help-window is live and in current frame
      ymacs-editor-display-help-window

    ;; delete old window if it is not in current frame
    (when (window-live-p ymacs-editor-display-help-window)
      (delete-window ymacs-editor-display-help-window)
      (setq ymacs-editor-display-help-window nil))

    (with-selected-window
        (setq ymacs-editor-display-help-window
              (let ((ignore-window-parameters t))
                (split-window
                 (frame-root-window) -1 'below)))
      (let (buffer)
        (if (setq buffer (get-buffer ymacs-editor-display-help-buffer-name))
            (switch-to-buffer buffer 'norecord)
          (switch-to-buffer ymacs-editor-display-help-buffer-name 'norecord)
          (fundamental-mode)
          (setq window-size-fixed t)
          (setq mode-line-format nil)
          (setq header-line-format nil)
          (setq tab-line-format nil)
          (setq cursor-type nil)
          (setq display-line-numbers nil)
          (setq display-fill-column-indicator nil)

          (page-break-lines-mode 1)))

      (let ((window (selected-window)))
        (set-window-hscroll window 0)
        (set-window-parameter window 'no-delete-other-windows t)
        (set-window-dedicated-p window t)
        (set-window-parameter window 'no-other-window t)))

    ymacs-editor-display-help-window))

(defun ymacs-editor//display-help--show (-message)
  (with-selected-window (ymacs-editor//display-help--get-window)
    (unless (string= (buffer-string) -message)
      (erase-buffer)
      (insert -message)

      (let ((window-resize-pixelwise t)
            (window-size-fixed nil))
        (fit-window-to-buffer nil nil 1)))
    (goto-char (point-min))))

(defun ymacs-editor//display-help--hide (&rest _)
  (when-let (buffer (get-buffer ymacs-editor-display-help-buffer-name))
    (mapc #'delete-window (get-buffer-window-list buffer)))
  (when (window-live-p ymacs-editor-display-help-window)
    (delete-window ymacs-editor-display-help-window)))

(defun ymacs-editor//display-help (&optional -keys -command -directory)
  (when (and -command (not (stringp -command)))
    (setq -command (string-join -command " ")))

  (when -command
    (setq -command (propertize -command 'face font-lock-doc-face)))

  (let ((max-cmd-length (frame-width)))
    (when (> (length -command) max-cmd-length)
      (setq -command (concat (substring -command 0 max-cmd-length)
                             (propertize "[...]" 'face font-lock-keyword-face)))))

  (let* ((extra-string
          (when ymacs-editor-display-help-extra-lines
            (propertize (string-join ymacs-editor-display-help-extra-lines "\n")
                        'face 'font-lock-string-face)))
         (key-string (ymacs-editor//display-help--keys -keys))
         (cmd-string
          (when -command
            (format "(@%s) %s"
                    (propertize (or -directory default-directory "") 'face font-lock-constant-face)
                    -command)))
         (help-string
          (concat extra-string
                  (when (and extra-string (or key-string cmd-string)) "\n")
                  key-string
                  (when (and cmd-string key-string) "\n")
                  cmd-string)))
    (unless (string-empty-p help-string)
      (ymacs-editor//display-help--show help-string))))
