;;; -*- lexical-binding: t; -*-

(after! cmake-mode
  (define-hook! ymacs-cpp-cmake|setup (cmake-mode-hook)
    (when (buffer-enable-rich-feature-p)
      (try-enable-lsp! cmake
        :-fallback (ymacs-editor//add-company-backend 'company-cmake)))))
