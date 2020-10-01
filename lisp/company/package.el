;;; -*- lexical-binding: t; -*-

(define-option! ymacs-company-modern-ui nil)

(require-packages!
 (company-posframe :when ymacs-company-modern-ui))

(make-variable-buffer-local 'company-backends)

(define-key!
  ("C-c <tab>" . company-complete)
  ("C-c TAB" . company-complete)
  ("C-c F" . company-files)

  ("C-}" . ymacs-company/yasnippet)
  ([f6] . ymacs-company/toggle-ispell))