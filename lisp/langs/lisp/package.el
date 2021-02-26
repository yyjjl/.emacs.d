;; -*- lexical-binding:t -*-

(require-packages!
 ;; Auto compile after .el file load or save
 auto-compile
 ;; pair edit
 lispy
 elisp-def
 macrostep)

(defface lisp-argument-face
  `((t :weight bold :italic t))
  "Face for arguments"
  :group 'lisp)

(autoload #'lispy-forward "lispy" nil t)
(autoload #'lispy-backward "lispy" nil t)
