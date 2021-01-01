;;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'hydra))

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

(defun ymacs-hydra-add-toggle-column (column)
  (setf (alist-get (car column) ymacs-hydra-local-toggles-heads-list)
        (cdr column)))
