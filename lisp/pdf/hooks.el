;; -*- lexical-binding: t; -*-

(define-hook! ymacs-pdf|after-init ((after-init-hook :append t))
  (require 'saveplace-pdf-view)

  (remove-hook
   'file-name-handler-alist
   (cons ymacs-external-file-regexp #'ymacs//external-file-handler))

  (add-hook
   'file-name-handler-alist
   (cons (ymacs//make-external-file-extensions-regexp
          (delete "pdf" ymacs-external-file-extensions))
         #'ymacs//external-file-handler)))
