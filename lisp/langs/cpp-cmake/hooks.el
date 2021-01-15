;;; -*- lexical-binding: t; -*-

(after! cmake-mode
  (define-hook! ymacs-cpp-cmake//setup (cmake-mode-hook)
    (when (is-buffer-suitable-for-coding!)
      (try-enable-lsp! cmake
        :-fallback (ymacs-editor//add-company-backend 'company-cmake)))))
