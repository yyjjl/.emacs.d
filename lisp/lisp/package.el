;; -*- lexical-binding:t -*-

(require-packages!
 ;; Auto compile after .el file load or save
 auto-compile
 ;; pair edit
 lispy
 elisp-def
 macrostep)

(defface lisp-argument-face
  `((t :underline t))
  "Face for arguments"
  :group 'lisp)
