;;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'hydra))

(defvar ymacs-editor-toggles-alist
  '(("Global"
     (t
      ("V"
       (ymacs-editor-view-code-mode (if ymacs-editor-view-code-mode -1 1))
       "View Code"
       :toggle ymacs-editor-view-code-mode)
      ("E"
       toggle-debug-on-error
       "Debug on Error"
       :toggle (default-value 'debug-on-error))
      ("Q"
       toggle-debug-on-quit
       "Debug on Quit"
       :toggle (default-value 'debug-on-quit))
      ("W"
       (setq show-trailing-whitespace (not show-trailing-whitespace))
       "Trailing Whitespace"
       :toggle show-trailing-whitespace)
      ("N"
       (display-line-numbers-mode (if display-line-numbers-mode -1 1))
       "Line Number"
       :toggle display-line-numbers-mode)
      ("B" display-battery-mode "Battery" :toggle t)
      ("T" display-time-mode "Time" :toggle t)
      ("L"
       toggle-truncate-lines
       "Truncate Line"
       :toggle truncate-lines)
      ("P" prettify-symbols-mode "Pretty Symbol" :toggle t)))))

(defmacro defhydra++ (name body &optional docstring &rest heads)
  "Redefine an existing hydra by adding new heads.
Arguments are same as of `defhydra'."
  (declare (indent defun) (doc-string 3))
  (unless (stringp docstring)
    (setq heads (cons docstring heads))
    (setq docstring nil))
  (let ((ignore-keys (mapcar #'car (--filter (memq :delete it) heads))))
    `(defhydra ,name ,(or body (hydra--prop name "/params"))
       ,(or docstring (hydra--prop name "/docstring"))
       ,@(cl-delete-duplicates
          (cl-delete-if
           (lambda (x) (member (car x) ignore-keys))
           (append (hydra--prop name "/heads") heads))
          :key #'car
          :test #'equal))))

(defun ymacs-editor//add-toggles (-column-name -condition &rest -toggles)
  (let* ((column (assoc-string -column-name ymacs-editor-toggles-alist))
         (group (assoc -condition column))
         (toggles (cdr group)))
    (dolist (toggle (reverse -toggles))
      (setf (alist-get (car toggle) toggles nil nil #'equal)
            (cdr toggle)))
    (if group
        (setcdr group toggles)
      (setq group (cons -condition toggles))
      (if column
          (setcdr (last column) (list group))
        (setq column (list -column-name group))
        (setcdr (last ymacs-editor-toggles-alist) (list column))))))
