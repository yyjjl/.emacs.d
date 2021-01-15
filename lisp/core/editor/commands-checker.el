;;; -*- lexical-binding: t; -*-

(defun ymacs-editor//ivy-error-transformer (-err)
  "Return a string of message constructed from ERROR."
  (let ((face (-> -err
                  flycheck-error-level
                  flycheck-error-level-error-list-face)))
    (format "%s:%s:[%s]:%s"
            (propertize (flycheck-error-filename -err)
                        'error -err)
            (propertize (number-to-string (flycheck-error-line -err))
                        'face 'flycheck-error-list-line-number)
            (propertize (symbol-name (flycheck-error-level -err))
                        'font-lock-face face)
            (or (flycheck-error-message -err) ""))))

;;;###autoload
(defun ymacs-editor/sudo-edit-or-errors ()
  (interactive)
  (let ((filename
         (if current-prefix-arg
             (or (buffer-file-name)
                 (read-file-name "Find file as root: " nil nil :must-match))
           (buffer-file-name))))
    (if (and filename
             (not (file-writable-p filename)))
        (counsel-find-file-as-root filename)
      (ivy-read "Errors: " (append
                            (mapcar #'ymacs-editor//ivy-error-transformer flycheck-current-errors)
                            (counsel-compilation-errors-cands))
                :require-match t
                :action (lambda (err)
                          (if (get-text-property 0 'error err)
                              (counsel-flycheck-errors-action err)
                            (counsel-compilation-errors-action err)))
                :caller 'counsel-flycheck
                :history 'counsel-flycheck-errors-history))))
