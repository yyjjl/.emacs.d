;; -*- lexical-binding:t -*-

(require-packages!
 ;; Auto compile after .el file load or save
 auto-compile
 elisp-def
 macrostep)

(defface lisp-argument-face
  `((t :weight bold :italic t))
  "Face for arguments"
  :group 'lisp)
