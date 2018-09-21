;;; -*- lexical-binding: t; -*-

(defun i3-msg//command (&rest -args)
  (let ((result (with-output-to-string
                  (with-current-buffer standard-output
                    (apply #'call-process "i3-msg" nil t nil -args))))
        (json-array-type 'list)
        (json-object-type 'hash-table)
        (json-false nil))
    (json-read-from-string result)))

(defun i3-msg//workspaces (&optional -property)
  (--filter (or (eq -property t)
                (gethash -property it))
            (i3-msg//command "-t" "get_workspaces")))

(defun i3-msg//get-workspaces-from-tree (-tree -names &optional -window-list)
  (let ((name (gethash "name" -tree)))
    (if (member name -names)
        (push (cons name -tree) -window-list)
      (dolist (node (gethash "nodes" -tree))
        (setq -window-list
              (i3-msg//get-workspaces-from-tree node -names -window-list)))))
  -window-list)

(defun i3-msg//windows-of-tree (-tree &optional -window-list)
  (-if-let (nodes (gethash "nodes" -tree))
      (dolist (node nodes)
        (setq -window-list (i3-msg//windows-of-tree node -window-list)))
    (unless (or (equal "dockarea" (gethash "layout" -tree))
                (string-prefix-p "i3bar for output"
                                 (gethash "name" -tree ""))
                (not (gethash "window" -tree)))
      (push -tree -window-list)))
  -window-list)

(defun i3-msg//windows (&optional -workspace-property -filter)
  (let ((names (mapcar
                (lambda (ws) (gethash "name" ws))
                (i3-msg//workspaces -workspace-property)))
        (tree (i3-msg//command "-t" "get_tree")))
    (cl-loop for (name . tree) in
             (i3-msg//get-workspaces-from-tree tree names)
             nconc
             (cl-loop for window in (i3-msg//windows-of-tree tree)
                      when (or (not -filter) (funcall -filter window))
                      collect (cons name window)))))

(defun i3-msg//focus-window (-window)
  (i3-msg//command (format "[id=%s]" (gethash "window" -window)) "focus"))

(defun i3//visible-frame-list ()
  (let ((window-id->frame (--map (cons (frame-parameter it 'outer-window-id) it)
                                 (visible-frame-list))))
    (cl-loop for (_ . window) in (i3-msg//windows "visible")
             for window-id = (format "%s" (gethash "window" window))
             for frame = (cdr-safe (assoc window-id window-id->frame))
             when frame
             collect frame)))



(defun i3//window-candidates (&optional -workspace-property)
  (cl-loop for (name . window) in (i3-msg//windows -workspace-property)
           for properies = (gethash "window_properties" window)
           for trimed-name = (string-join (cdr (split-string name ":")) ":")
           collect (cons
                    (concat (propertize trimed-name
                                        'face font-lock-builtin-face)
                            " "
                            (propertize (gethash "class" properies "Unknown")
                                        'face font-lock-constant-face)
                            " "
                            (gethash "title" properies ""))
                    window)))

(defmacro i3//run-command (&rest body)
  `(unwind-protect
       (progn ,@body)
     (save-buffers-kill-terminal t)))

(defun i3/goto-window ()
  (interactive)
  (ivy-read "Goto Window: " (i3//window-candidates t)
            :require-match t
            :action (lambda (x)
                      (i3-msg//focus-window (cdr x)))))

(defun i3/other-frame ()
  (interactive)
  (let ((frames (delq (selected-frame) (i3//visible-frame-list)))
        (frame (next-frame (selected-frame))))
    (while (not (memq frame frames))
      (setq frame (next-frame frame)))
    (select-frame-set-input-focus frame)))

(provide 'i3wm-lib)
