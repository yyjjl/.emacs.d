;;; -*- lexical-binding: t; -*-

(define-hook! ymacs-cpp-cmake//setup (cmake-mode-hook cmake-ts-mode-hook)
  (with-transient-hook! (hack-local-variables-hook :local t)
    (when (is-buffer-suitable-for-coding!)
      (eval-when-has-feature! lsp
        (ymacs-lsp//try-enable cmake)))))
