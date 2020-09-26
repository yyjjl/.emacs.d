;; -*- lexical-binding:t -*-

(define-hook! ymacs-company|after-init (after-init-hook)
  (global-company-mode 1))
