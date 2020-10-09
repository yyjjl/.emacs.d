;; -*- lexical-binding:t -*-

(add-hook 'after-init-hook #'which-key-mode)

(define-hook! (ymacs-company|after-make-frame _) (after-make-frame-functions)
  (if (display-graphic-p)

      (when (not company-posframe-mode)
        (company-posframe-mode 1))

    (when company-posframe-mode
      (company-posframe-mode -1))))
