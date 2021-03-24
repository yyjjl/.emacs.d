;;; -*- lexical-binding: t; -*-

(defconst ymacs-desktop-directory (expand-cache! "desktop"))
(defvar ymacs-desktop-name nil)

(setq desktop-restore-frames nil)
(setq desktop-globals-to-save
      '(desktop-missing-file-warning
        tags-file-name
        tags-table-list
        register-alist))
(setq desktop-globals-to-clear nil)
(setq desktop-save 'if-exists)

(after! desktop
  (defmacro ymacs-desktop//without-semantic-mode (&rest -body)
    (declare (indent 0))
    `(let ((semantic-enable-p (bound-and-true-p semantic-mode)))
       (when semantic-enable-p
         (semantic-mode -1))
       ,@-body
       (when semantic-enable-p
         (semantic-mode 1))))

  (define-advice desktop-buffer-info (:around (-fn -buffer) no-minor-modes)
    (let ((info (funcall -fn -buffer)))
      (setf (nth 4 info) nil)
      info))

  (define-advice desktop-read (:around (-fn &rest -args) disable-semantic)
    "Temporarily disable semantic mode when load desktop"
    (ymacs-desktop//without-semantic-mode
      (apply -fn -args))))
