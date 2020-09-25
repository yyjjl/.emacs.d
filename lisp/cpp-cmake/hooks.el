;;; -*- lexical-binding: t; -*-

(after! cmake-mode
  (define-hook! ymacs-cpp-cmake|setup (cmake-mode-hook)
    (font-lock-mode 1)
    (cmake-font-lock-activate)

    (ymacs-lsp//try-enable cmake
      :fallback
      (ymacs-company//add-backend 'company-cmake))))
