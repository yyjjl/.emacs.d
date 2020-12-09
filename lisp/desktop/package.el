;; -*- lexical-binding:t -*-

(defvar ymacs-tools-current-desktop-name nil)

(define-key!
  ("C-x , c" . ymacs-tools/change-or-new-desktop)
  ("C-x , d" . ymacs-tools/delete-desktop))
