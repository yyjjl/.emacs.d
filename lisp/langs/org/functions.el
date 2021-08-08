;;; -*- lexical-binding: t; -*-

(defmacro ymacs-org/by-backend (&rest -body)
  `(case (or (and (boundp 'org-export-current-backend)
                  org-export-current-backend)
             'babel)
     ,@-body))

(defun ymacs-org/block-speed-command-activate (keys)
  "Hook for activating single-letter block commands."
  (when (and (bolp)
             (looking-at
              (eval-when-compile
                (concat
                 "^[\t ]*#\\+begin_?\\([^ \n]+\\)\\(\\([^\n]+\\)\\)?"
                 "\\|"
                 "^[\t ]*#\\+end_?\\([^ \n]+\\)$"))))
    (cdr (assoc keys ymacs-org-block-key-bindings))))

(defun ymacs-org@wrap-publish-fn (-fn -plist -filename -pub-dir)
  (condition-case err
      (let ((org-confirm-babel-evaluate))
        (funcall -fn -plist -filename -pub-dir))
    (user-error (message "Fail to publish file %s: %s" -filename err))))
