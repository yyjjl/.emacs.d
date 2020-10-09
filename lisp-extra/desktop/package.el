;; -*- lexical-binding:t -*-

(defvar ymacs-misc-current-desktop-name nil)

(define-key!
  (", c" . ymacs-misc/change-or-new-desktop)
  (", d" . ymacs-misc/delete-desktop))
