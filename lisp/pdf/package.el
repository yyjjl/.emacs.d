;; -*- lexical-binding: t; -*-

(require-packages!
 pdf-tools
 org-pdftools
 saveplace-pdf-view)

(define-key!
  ("C-x P" . ymacs-pdf/open-pdfs))
