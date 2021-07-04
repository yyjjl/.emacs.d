;;; -*- lexical-binding: t; -*-

(after! cmake-mode
  (define-hook! ymacs-cpp-cmake//setup (cmake-mode-hook)
    (with-transient-hook! (hack-local-variables-hook :local t)
      (unless (and (is-buffer-suitable-for-coding!)
                   (eval-when-has-feature! lsp
                     (ymacs-lsp//try-enable cmake)))
        (ymacs-editor//add-company-backend 'company-cmake)))))
