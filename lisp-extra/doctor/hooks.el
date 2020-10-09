;;; -*- lexical-binding: t; -*-

(define-hook! :anonymous (after-init-hook)
  (ymacs-doctor//start-doctor-timer 60 3600 900 60))
