;; -*- lexical-binding:t -*-

(eval-when-has-feature! lsp
  (after! sh-script
    (when (and (memq 'sh-mode ymacs-native-treesit-modes)
               (boundp 'bash-ts-mode-map))
      (set-keymap-parent bash-ts-mode-map sh-mode-map))

    (define-hook! ymacs-sh//setup (sh-mode-hook bash-ts-mode-hook)
      (ymacs-lsp//try-enable-eglot sh))))
