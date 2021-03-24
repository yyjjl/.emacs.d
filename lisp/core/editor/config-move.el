;;; -*- lexical-binding: t; -*-

(defvar ymacs-editor-forward-sexp-handler nil)

(cl-defmacro ymacs-editor//set-forward-sexp-handler
    (&key ((:modes -modes))
          ((:forward -forward-fn))
          ((:backward -backward-fn)))
  `(dolist (mode ',-modes)
     (setf (alist-get mode ymacs-editor-forward-sexp-handler) (cons ,-forward-fn ,-backward-fn))))
