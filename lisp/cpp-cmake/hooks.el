;;; -*- lexical-binding: t; -*-

(after! cmake-mode
  (define-hook! ymacs-cpp-cmake|setup (cmake-mode-hook)
    (font-lock-mode 1)
    (cmake-font-lock-activate)

    (when (buffer-enable-rich-feature-p)
      (try-enable-lsp! cmake
        :fallback (ymacs-company//add-backend 'company-cmake)))))
