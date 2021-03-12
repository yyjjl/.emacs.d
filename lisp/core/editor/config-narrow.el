;;; -*- lexical-binding: t; -*-

(defvar ymacs-editor-narrow-dwim-alist nil)

(cl-defmacro ymacs-editor//set-narrow-handler
    (&key ((:mode -major-mode)) ((:fn1 -narrow-fn-1)) ((:fn2 -narrow-fn-2)))
  `(add-to-list 'ymacs-editor-narrow-dwim-alist '(,-major-mode ,-narrow-fn-1 ,-narrow-fn-2)))
