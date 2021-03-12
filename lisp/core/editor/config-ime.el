;;; -*- lexical-binding: t; -*-

(defvar ymacs-editor-input-method-alist ())

(define-advice toggle-input-method (:around (-fn &rest -args) preload)
  (if-let (item (alist-get major-mode ymacs-editor-input-method-alist))
      (when (require (car item) nil t)
        (let ((default-input-method (cadr item)))
          (apply -fn -args)))
    (apply -fn -args)))

(cl-defmacro ymacs-editor//set-input-method
    (&key ((:mode -major-mode)) ((:ime -ime)) ((:package -pkg)))
  `(add-to-list 'ymacs-editor-input-method-alist '(,-major-mode ,-ime ,-pkg)))
