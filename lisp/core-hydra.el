;;; -*- lexical-binding: t; -*-

(autoload 'hydra-resize-window/shrink-window "autoloads/hydra" nil t)
(autoload 'hydra-resize-window/shrink-window-horizontally "autoloads/hydra" nil t)
(autoload 'hydra-resize-window/enlarge-window "autoloads/hydra" nil t)
(autoload 'hydra-resize-window/enlarge-window-horizontally "autoloads/hydra" nil t)

(autoload 'hydra-global-toggles/body "autoloads/hydra" nil t)

(autoload 'hydra-ediff/body "autoloads/hydra" nil t)
(autoload 'hydra-outline/body "autoloads/hydra" nil t)
(autoload 'hydra-rectangle/body "autoloads/hydra" nil t)
(autoload 'hydra-sort/body "autoloads/hydra" nil t)
(autoload 'hydra-next-error/body "autoloads/hydra" nil t)

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

(define-key!
  ("C-x {" . hydra-resize-window/shrink-window-horizontally)
  ("C-x }" . hydra-resize-window/enlarge-window-horizontally)
  ("C-x ^" . hydra-resize-window/enlarge-window)
  ("C-x -" . hydra-resize-window/shrink-window)

  ([M-f11] . scroll-other-window-down)
  ([M-f12] . scroll-other-window)

  ([f7] . hydra-global-toggles/body)

  ("C-c O" . hydra-outline/body)
  ("C-x SPC" . hydra-rectangle/body)

  ("C-x , s" . hydra-sort/body)
  ("C-x , e" . hydra-ediff/body)

  ("C-x `" . hydra-next-error/body))

(provide 'core-hydra)
