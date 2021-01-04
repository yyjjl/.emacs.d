;;; -*- lexical-binding: t; -*-

(defun ymacs-editor//ivy-error-transformer (-err)
  "Return a string of message constructed from ERROR."
  (let ((face (-> -err
                  flycheck-error-level
                  flycheck-error-level-error-list-face)))
    (propertize
     (format "%-8s L%-5s C%-3s %s"
             (propertize (upcase (symbol-name (flycheck-error-level -err)))
                         'font-lock-face face)
             (propertize (number-to-string (flycheck-error-line -err))
                         'face 'flycheck-error-list-line-number)
             (propertize (-if-let (column (flycheck-error-column -err))
                             (number-to-string column)
                           "0")
                         'face 'flycheck-error-list-column-number)
             (or (flycheck-error-message -err) ""))
     'error -err)))

(defun ymacs-editor//get-errors (-only-error)
  (mapcar
   #'ymacs-editor//ivy-error-transformer
   (--filter
    (or (not -only-error)
        (>=
         (flycheck-error-level-severity (flycheck-error-level it))
         (flycheck-error-level-severity 'error)))
    flycheck-current-errors)))

;;;###autoload
(defun ymacs-editor/jump-to-error (-only-error)
  "Flycheck errors."
  (interactive "P")
  (require 'flycheck)
  (let ((current-point (point))
        (lineno (line-number-at-pos))
        (errors (ymacs-editor//get-errors -only-error)))
    (condition-case nil
        (ivy-read "flycheck errors: " errors
                  :preselect
                  (--find
                   (ignore-errors
                     (>= (flycheck-error-line (get-text-property 0 'error it))
                         lineno))
                   errors)
                  :require-match t
                  :action #'counsel-flycheck-errors-action
                  :history 'counsel-flycheck-errors-history
                  :caller 'counsel-flycheck
                  :keymap (define-key! :map (make-sparse-keymap)
                            ("C-n" . ivy-next-line-and-call)
                            ("C-p" . ivy-previous-line-and-call)))
      (quit (goto-char current-point)))))
