;; -*- lexical-binding:t -*-

(define-hook! ymacs-company|after-init (after-init-hook)
  (global-company-mode 1))

(define-hook! (ymacs-company|after-make-frame _) (after-make-frame-functions)
  (when ymacs-company-modern-ui
    (if (display-graphic-p)

        (when (not company-posframe-mode)
          (company-posframe-mode 1))

      (when company-posframe-mode
        (company-posframe-mode -1)))))
