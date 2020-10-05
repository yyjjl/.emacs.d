;; -*- lexical-binding: t; -*-

(define-hook! ymacs-pdf|after-init ((after-init-hook :append t))
  (require 'pdf-tools)

  (pdf-tools-install-noverify)

  (require 'saveplace-pdf-view)

  (remove-hook
   'file-name-handler-alist
   (cons ymacs-external-file-regexp #'ymacs//external-file-handler))

  (add-hook
   'file-name-handler-alist
   (cons (ymacs//make-external-file-extensions-regexp
          (delete "pdf" ymacs-external-file-extensions))
         #'ymacs//external-file-handler)))

(after! saveplace-pdf-view
  (advice-add #'saveplace-pdf-view-to-alist-advice :around #'ignore-errors!))
