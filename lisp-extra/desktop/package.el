;; -*- lexical-binding:t -*-

(defvar ymacs-tools-current-desktop-name nil)

(define-key!
  (", c" . ymacs-tools/change-or-new-desktop)
  (", d" . ymacs-tools/delete-desktop))
